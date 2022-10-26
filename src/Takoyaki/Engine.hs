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
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Ki
import Lucid (Attribute, Html, ToHtml (toHtml), With (with), renderBS)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid (HTML)
import Servant.XStatic (xstaticServant)
import Takoyaki.Htmx
import qualified XStatic.Tailwind as XStatic
import Prelude

data App s se = App
  { appName :: Text,
    appGenState :: IO s,
    appRender :: TVar s -> STM (Html ()),
    appHandleEvent :: WSEvent -> TVar s -> TBQueue se -> IO [Html ()],
    appService :: TVar s -> TBQueue se -> WS.Connection -> IO ()
  }

kiConcurrently :: IO a -> IO b -> IO ()
kiConcurrently a1 a2 =
  Ki.scoped $ \scope -> do
    t1 <- Ki.fork scope a1
    void a2
    void $ atomically (Ki.await t1)
    pure ()

connectionHandler :: App s se -> WS.Connection -> Handler ()
connectionHandler app conn = liftIO $ do
  WS.withPingThread conn 5 (pure ()) $ do
    putStrLn [i|New connection ...|]
    s <- app.appGenState
    state <- newTVarIO s
    serviceQ <- newTBQueueIO 1
    appDom <- atomically $ app.appRender state
    WS.sendTextData conn . renderBS . div_ [id_ "init"] $ appDom
    kiConcurrently (handleEvents state serviceQ) (app.appService state serviceQ conn)
  where
    handleEvents state serviceQ = forever $ do
      msg <- WS.receiveDataMessage conn
      case decodeWSEvent msg of
        Nothing -> pure ()
        Just wsEvent -> do
          putStrLn $ "Received WS event: " <> show wsEvent
          fragments <- app.appHandleEvent wsEvent state serviceQ
          mapM_ (WS.sendTextData conn . renderBS) fragments

withEvent :: TriggerId -> [Attribute] -> Html () -> Html ()
withEvent tId tAttrs elm = with elm ([id_ tId, wsSend ""] <> tAttrs)

bootHandler :: Text -> Html ()
bootHandler title = do
  doctypehtml_ $ do
    head_ $ do
      title_ $ (toHtml title)
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts $ xStaticFiles <> [XStatic.tailwind]
    body_ $ do
      div_ [class_ "container mx-auto", hxExtWS, wsConnect "/ws"] $
        div_ [id_ "init"] ""

type API =
  "xstatic" :> Raw
    :<|> Get '[HTML] (Html ())
    :<|> "ws" :> WebSocket

appServer :: App s se -> Server API
appServer app =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> pure (bootHandler app.appName)
    :<|> connectionHandler app

runServer :: App s se -> IO ()
runServer app = Warp.run 8092 $ serve (Proxy @API) $ appServer app
