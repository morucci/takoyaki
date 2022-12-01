module Takoyaki.Engine
  ( App (..),
    runServer,
    WSEvent (..),
    withEvent,
    getFromJSON,
  )
where

import Codec.Serialise
import Control.Concurrent.STM
import Control.Exception.Safe (SomeException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID, fromASCIIBytes)
import qualified Database.SQLite.Simple as DB
import qualified Ki
import Lucid (Attribute, Html, ToHtml (toHtml), With (with), renderBS)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.Auth.Server (SetCookie)
import Servant.HTML.Lucid (HTML)
import Servant.XStatic (xstaticServant)
import System.Directory
import System.FilePath.Posix ((</>))
import System.Random (randomIO)
import Takoyaki.Htmx
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, sameSiteLax)
import Witch
import qualified XStatic.Tailwind as XStatic
import Prelude

data App s se = App
  { appName :: Text,
    appMkSessionState :: IO s,
    appInitDB :: DB.Connection -> IO (),
    appRender :: TVar s -> DB.Connection -> IO (Html ()),
    appHandleEvent :: WSEvent -> TVar s -> TBQueue se -> DB.Connection -> IO [Html ()],
    appService :: TVar s -> TBQueue se -> WS.Connection -> IO ()
  }

connectionHandler :: App s se -> SessionStore s -> Maybe UUID -> WS.Connection -> Handler ()
connectionHandler _ _ Nothing _ = error "Missing session UUID"
connectionHandler app storeV (Just sessionUUID) conn = liftIO $ do
  WS.withPingThread conn 5 (pure ()) $ do
    putStrLn [i|New connection #{sessionUUID}...|]
    sE <- storeV.loadSession sessionUUID
    state <- case sE of
      Left _ -> newTVarIO =<< app.appMkSessionState
      Right prevState -> newTVarIO prevState
    serviceQ <- newTBQueueIO 1
    appDom <- withAppDBConnection app.appName $ app.appRender state
    WS.sendTextData conn . renderBS . div_ [id_ "init"] $ appDom
    Ki.scoped $ \scope -> do
      serviceT <- Ki.fork scope (app.appService state serviceQ conn)
      void $ handleEvents state serviceQ
      atomically $ Ki.await serviceT
  where
    handleEvents stateV serviceQ = withAppDBConnection app.appName $
      \dbConn -> forever $ do
        msg <- WS.receiveDataMessage conn
        case decodeWSEvent msg of
          Nothing -> pure ()
          Just wsEvent -> do
            putStrLn $ "Received WS event: " <> show wsEvent
            withAppDBConnection app.appName (const $ pure ())
            fragments <- app.appHandleEvent wsEvent stateV serviceQ dbConn
            state <- readTVarIO stateV
            void $ storeV.dumpSession sessionUUID state
            mapM_ (WS.sendTextData conn . renderBS) fragments

data SessionStore s = SessionStore
  { getSessionStorePath :: FilePath,
    loadSession :: UUID -> IO (Either SomeException s),
    dumpSession :: UUID -> s -> IO (Either SomeException ())
  }

mkSessionStore :: Serialise s => Text -> IO (SessionStore s)
mkSessionStore appName = do
  dataDir <-
    getXdgDirectory XdgData
      . from
      $ "takoyaki" </> from appName </> "sessions"
  createDirectoryIfMissing True dataDir
  pure $
    SessionStore
      { getSessionStorePath = dataDir,
        loadSession = loadSession dataDir,
        dumpSession = dumpSession dataDir
      }
  where
    mkPath path uuid = path </> (show uuid)
    loadSession path uuid =
      try $ readFileDeserialise $ mkPath path uuid
    dumpSession path uuid state =
      try $ writeFileSerialise (mkPath path uuid) state

withAppDBConnection :: Text -> (DB.Connection -> IO a) -> IO a
withAppDBConnection appName action = do
  dbPath <- getAppDBPath
  DB.withConnection dbPath action
  where
    getAppDBPath :: IO FilePath
    getAppDBPath = do
      dataDir <-
        getXdgDirectory XdgData
          . from
          $ "takoyaki" </> from appName
      createDirectoryIfMissing True dataDir
      pure $ dataDir </> "db.sqlite"

withEvent :: TriggerId -> [Attribute] -> Html () -> Html ()
withEvent tId tAttrs elm = with elm ([id_ tId, wsSend ""] <> tAttrs)

bootHandler :: Text -> Maybe Text -> Handler (Headers '[Header "Set-Cookie" SetCookie] (Html ()))
bootHandler title cookieHeaderM = do
  sessionUUID <- case getSessionUUID of
    Just uuid -> pure uuid
    Nothing -> liftIO genUUID
  pure $ do
    withSetCookie sessionUUID $ doctypehtml_ $ do
      head_ $ do
        title_ $ (toHtml title)
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        xstaticScripts $ xStaticFiles <> [XStatic.tailwind]
      body_ $ do
        div_ [class_ "container mx-auto", hxExtWS, wsConnect $ wsPath sessionUUID] $
          div_ [id_ "init"] ""
  where
    cookieName = "sessionUUID"
    wsPath :: UUID -> Text
    wsPath uuid = "/ws?sessionUUID=" <> (from $ show uuid)
    genUUID :: MonadIO m => m UUID
    genUUID = randomIO
    withSetCookie :: UUID -> Html () -> Headers '[Header "Set-Cookie" SetCookie] (Html ())
    withSetCookie uuid =
      let cookie =
            defaultSetCookie
              { setCookieName = cookieName,
                setCookieValue = encodeUtf8 . from $ show uuid,
                setCookieSameSite = Just sameSiteLax,
                setCookieHttpOnly = True
              }
       in addHeader cookie
    getSessionUUID :: Maybe UUID
    getSessionUUID = do
      let parsedCookies = (parseCookies . encodeUtf8) <$> (cookieHeaderM)
      case parsedCookies of
        Just cookies -> case filter (\(k, _) -> k == cookieName) cookies of
          [] -> Nothing
          [x] -> fromASCIIBytes $ snd x
          (_ : _) -> Nothing -- More than one cookie found so consider nothing
        Nothing -> Nothing

type API =
  "xstatic" :> Raw
    :<|> Header "Cookie" Text :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie] (Html ()))
    :<|> QueryParam "sessionUUID" UUID :> "ws" :> WebSocket

appServer :: App s se -> SessionStore s -> Server API
appServer app store =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> bootHandler app.appName
    :<|> connectionHandler app store

runServer :: Serialise s => Int -> App s se -> IO ()
runServer port app = do
  sessionStore <- mkSessionStore app.appName
  void $ withAppDBConnection app.appName app.appInitDB
  Warp.run port $ serve (Proxy @API) $ appServer app sessionStore
