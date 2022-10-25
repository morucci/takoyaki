module Takoyaki.Engine
  ( App (..),
    runServer,
    WSEvent (..),
    withEvent,
    getFromJSON,
    AppQ,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM (STM, TBQueue, TVar, atomically, newTBQueue, readTBQueue, writeTBQueue)
import Control.Monad (void)
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

data App s ev = App
  { appName :: Text,
    appWSEvent :: WSEvent -> Maybe ([IO ev]),
    appState :: TVar s,
    appRender :: TVar s -> STM (Html ()),
    appHandleEvent :: ev -> TVar s -> IO [Html ()]
  }

type AppQ ev = TBQueue ([IO ev])

connectionHandler :: Show ev => App s ev -> WS.Connection -> Handler ()
connectionHandler app conn = do
  appQ <- liftIO . atomically $ initAppQ
  void . liftIO . runConcurrently $
    (,)
      <$> Concurrently (handleR appQ)
      <*> Concurrently (handleS appQ)
  where
    initAppQ :: STM (AppQ ev)
    initAppQ = newTBQueue 10

    handleS appQ = do
      evIOs <- atomically $ readTBQueue appQ
      mapM_ handleEV evIOs
      handleS appQ
      where
        handleEV eio = do
          event <- eio
          putStrLn $ "Handling event: " <> show event
          fragments <- app.appHandleEvent event app.appState
          mapM_ (WS.sendTextData conn . renderBS) fragments

    handleR appQ = do
      liftIO $ WS.withPingThread conn 5 (pure ()) $ do
        putStrLn [i|New connection ...|]
        appDom <- atomically $ app.appRender app.appState
        WS.sendTextData conn . renderBS . div_ [id_ "init"] $ appDom
        handleClient
      where
        handleClient = do
          msg <- WS.receiveDataMessage conn
          handleDataMessage msg
          handleClient
        handleDataMessage msg = do
          case decodeWSEvent msg of
            Nothing -> pure ()
            Just wsEvent -> do
              putStrLn $ "Received WS event: " <> show wsEvent
              atomically $ mapM_ (writeTBQueue appQ) $ app.appWSEvent wsEvent

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

appServer :: Show ev => App s ev -> Server API
appServer app =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> pure (bootHandler app.appName)
    :<|> connectionHandler app

runServer :: Show ev => App s ev -> IO ()
runServer app = Warp.run 8092 $ serve (Proxy @API) $ appServer app
