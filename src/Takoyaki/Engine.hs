module Takoyaki.Engine
  ( App (..),
    runServer,
    WSEvent (..),
    withEvent,
    getFromJSON,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
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

data App s = App
  { appName :: Text,
    appState :: TVar s,
    appRender :: TVar s -> STM (Html ()),
    appHandleEvent :: WSEvent -> TVar s -> IO [Html ()]
  }

connectionHandler :: App s -> WS.Connection -> Handler ()
connectionHandler app conn = liftIO $ do
  WS.withPingThread conn 5 (pure ()) $ do
    putStrLn [i|New connection ...|]
    appDom <- atomically $ app.appRender app.appState
    WS.sendTextData conn . renderBS . div_ [id_ "init"] $ appDom
    handleEvents
  where
    handleEvents = forever $ do
      msg <- WS.receiveDataMessage conn
      case decodeWSEvent msg of
        Nothing -> pure ()
        Just wsEvent -> do
          putStrLn $ "Received WS event: " <> show wsEvent
          fragments <- app.appHandleEvent wsEvent app.appState
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

appServer :: App s -> Server API
appServer app =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> pure (bootHandler app.appName)
    :<|> connectionHandler app

runServer :: App s -> IO ()
runServer app = Warp.run 8092 $ serve (Proxy @API) $ appServer app
