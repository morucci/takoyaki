module Takoyaki.Engine
  ( App (..),
    runServer,
    WSEvent (..),
    withEvent,
    getFromJSON,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID, fromASCIIBytes)
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
import System.Random (randomIO)
import Takoyaki.Htmx
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, sameSiteLax)
import Witch
import qualified XStatic.Tailwind as XStatic
import Prelude

data App s se = App
  { appName :: Text,
    appGenState :: IO s,
    appRender :: TVar s -> STM (Html ()),
    appHandleEvent :: WSEvent -> TVar s -> TBQueue se -> IO [Html ()],
    appService :: TVar s -> TBQueue se -> WS.Connection -> IO ()
  }

type SessionStateStore s = TVar (Map.Map UUID s)

connectionHandler :: App s se -> SessionStateStore s -> Maybe UUID -> WS.Connection -> Handler ()
connectionHandler _ _ Nothing _ = error "Missing session UUID"
connectionHandler app storeV (Just sessionUUID) conn = liftIO $ do
  WS.withPingThread conn 5 (pure ()) $ do
    putStrLn [i|New connection #{sessionUUID}...|]
    sM <- loadClientState sessionUUID
    state <- case sM of
      Nothing -> newTVarIO =<< app.appGenState
      Just prevState -> newTVarIO prevState
    serviceQ <- newTBQueueIO 1
    appDom <- atomically $ app.appRender state
    WS.sendTextData conn . renderBS . div_ [id_ "init"] $ appDom
    Ki.scoped $ \scope -> do
      serviceT <- Ki.fork scope (app.appService state serviceQ conn)
      void $ handleEvents state serviceQ
      atomically $ Ki.await serviceT
  where
    handleEvents state serviceQ = forever $ do
      msg <- WS.receiveDataMessage conn
      case decodeWSEvent msg of
        Nothing -> pure ()
        Just wsEvent -> do
          putStrLn $ "Received WS event: " <> show wsEvent
          fragments <- app.appHandleEvent wsEvent state serviceQ
          dumpClientState sessionUUID state
          mapM_ (WS.sendTextData conn . renderBS) fragments
    loadClientState uuid = do
      atomically $ do
        store <- readTVar storeV
        pure $ Map.lookup uuid store
    dumpClientState uuid state = do
      void $ liftIO $ atomically $ do
        appState <- readTVar state
        modifyTVar' storeV $ \store -> Map.insert uuid appState store

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

appServer :: App s se -> SessionStateStore s -> Server API
appServer app store =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> bootHandler app.appName
    :<|> connectionHandler app store

runServer :: App s se -> IO ()
runServer app = do
  store <- newTVarIO Map.empty
  Warp.run 8092 $ serve (Proxy @API) $ appServer app store
