module Takoyaki.Engine
  ( App (..),
    runServer,
    WSEvent (..),
    withEvent,
    getFromJSON,
  )
where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (STM, TBQueue, atomically, newTBQueue, readTBQueue, writeTBQueue)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (State, evalState, runState)
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
    appState :: s,
    appRender :: State s (Html ()),
    -- TODO: change to: ev -> State s [Html ()]
    appHandleEvent :: ev -> State s (Html ())
  }

type AppQ ev = TBQueue ([IO ev])

connectionHandler :: Show ev => App s ev -> WS.Connection -> Handler ()
connectionHandler app conn = do
  queue <- liftIO . atomically $ initQ
  liftIO $ concurrently_ (handleR queue) (handleS queue app.appState)
  where
    initQ :: STM (AppQ ev)
    initQ = newTBQueue 10
    handleS queue appState = do
      evIOs <- atomically $ readTBQueue queue
      newState <- handleEVs appState evIOs
      handleS queue newState
      where
        handleEVs as eios = case eios of
          [] -> pure as
          [x] -> handleEV as x
          (x : xs) -> do
            ns <- handleEV as x
            handleEVs ns xs
        handleEV as eio = do
          event <- eio
          putStrLn $ "Handling event: " <> show event
          let (fragment, newState) = runState (app.appHandleEvent event) as
          WS.sendTextData conn . renderBS $ fragment
          pure newState

    handleR queue = do
      liftIO $ WS.withPingThread conn 5 (pure ()) $ do
        putStrLn [i|New connection ...|]
        let appDom = evalState app.appRender app.appState
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
              atomically $ do
                mapM_ (writeTBQueue queue) $ app.appWSEvent wsEvent

-- withEvent :: TriggerId -> Maybe Trigger -> [(Aeson.Key, Text)] -> Html () -> Html ()
-- withEvent tId triggerM vals elm =
--   let elm' = with elm [id_ tId, mkHxVals vals, wsSend ""]
--    in case triggerM of
--         Just trigger -> with elm' [hxTrigger trigger]
--         Nothing -> elm'

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
