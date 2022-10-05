module Takoyaki.Engine where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTBQueue, newTVar, readTBQueue, readTVar, writeTBQueue)
import Control.Monad.IO.Class (liftIO)
-- import Servant (Get, Handler, Raw, Server, type (:<|>), type (:>))

import Control.Monad.State (State, evalState, execState)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Lucid (Html, renderBS, with)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid (HTML)
import Servant.XStatic (xstaticServant)
import Takoyaki.Htmx
  ( Trigger,
    WSEvent (..),
    WSwapStrategy,
    WidgetId,
    decodeWSEvent,
    hxExtWS,
    hxSwapOOB,
    hxTrigger,
    hxVals,
    swapToText,
    wsConnect,
    wsSend,
    xStaticFiles,
  )
import qualified XStatic.Tailwind as XStatic
import Prelude

data WEvent = WEvent
  { eTarget :: WidgetId,
    eType :: Text
  }
  deriving (Show)

type WState = Aeson.Value

type WStore = Map.Map WidgetId Widget

type Registry = TVar WStore

type DomInit = Registry -> STM (Html ())

data Widget = Widget
  { wId :: WidgetId,
    wSwap :: WSwapStrategy,
    wsEvent :: WSEvent -> Maybe WEvent,
    wRender :: State (Maybe WState) (Html ()),
    wState :: Maybe WState,
    wStateUpdate :: WEvent -> State (Maybe WState) (),
    wTrigger :: Maybe (Maybe Trigger)
  }

instance Show Widget where
  show w = "Widget: " <> show w.wId <> " - State: " <> show w.wState

addWidget :: Registry -> Widget -> STM ()
addWidget st w = modifyTVar st $ Map.insert w.wId w

getWidget :: Registry -> WidgetId -> STM (Maybe Widget)
getWidget reg wid = do
  st <- readTVar reg
  pure $ Map.lookup wid st

initRegistry :: [Widget] -> STM Registry
initRegistry widgets = do
  reg <- newTVar Map.empty
  mapM_ (addWidget reg) widgets
  pure reg

renderWidget :: Registry -> WidgetId -> STM (Html ())
renderWidget reg wid = do
  st <- readTVar reg
  case Map.lookup wid st of
    Just w -> pure $ widgetRender w
    Nothing -> pure mempty

processEventWidget :: Registry -> WEvent -> Widget -> STM Widget
processEventWidget reg event w = do
  let newState = execState (w.wStateUpdate event) w.wState
      newWidget = w {wState = newState}
  addWidget reg newWidget
  pure newWidget

widgetRender :: Widget -> Html ()
widgetRender w = with elm [wIdVal, hxSwapOOB . swapToText $ wSwap w]
  where
    elm = case wTrigger w of
      Nothing -> with baseElm [id_ w.wId]
      Just triggerM -> withEvent w.wId triggerM baseElm
    baseElm = div_ $ evalState w.wRender w.wState
    wIdVal =
      hxVals
        . toStrict
        . Aeson.encodeToLazyText
        $ Aeson.fromList [("widgetId", w.wId)]

-- If trigger not specified then the fallback is the natural event
withEvent :: WidgetId -> Maybe Trigger -> Html () -> Html ()
withEvent eid triggerM elm =
  let elm' = with elm [id_ eid, wsSend ""]
   in case triggerM of
        Just trigger -> with elm' [hxTrigger trigger]
        Nothing -> elm'

expWSHandler :: Registry -> (Registry -> STM (Html ())) -> WS.Connection -> Handler ()
expWSHandler registry initDom conn = do
  queue <- liftIO . atomically $ newTBQueue 10
  liftIO $ concurrently_ (handleR queue) (handleS queue)
  where
    handleS queue = do
      widgetToRenderM <- atomically $ do
        event <- readTBQueue queue
        widgetM <- getWidget registry event.eTarget
        mapM (processEventWidget registry event) widgetM
      case widgetToRenderM of
        Just widgetToRender -> do
          putStrLn $ "Rendering widget: " <> show widgetToRender
          WS.sendTextData conn $ renderBS $ widgetRender widgetToRender
        Nothing -> pure ()
      handleS queue
    handleR queue = do
      liftIO $ WS.withPingThread conn 5 (pure ()) $ do
        putStrLn [i|New connection ...|]
        dom <- atomically $ initDom registry
        WS.sendTextData conn $ renderBS $ div_ [id_ "init"] dom
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
                targetedWidgetM <- getWidget registry wsEvent.wseWidgetId
                mapM_ (writeTBQueue queue) $ case targetedWidgetM of
                  Just targetedWidget -> targetedWidget.wsEvent wsEvent
                  Nothing -> Nothing

expHTMLHandler :: Html ()
expHTMLHandler = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Experiment"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts $ xStaticFiles <> [XStatic.tailwind]
    body_ $ do
      div_ [class_ "container mx-auto", hxExtWS, wsConnect "/ws"] $
        div_ [id_ "init"] ""

type ExpAPI =
  "xstatic" :> Raw
    :<|> Get '[HTML] (Html ())
    :<|> "ws" :> WebSocket

expServer :: Registry -> DomInit -> Server ExpAPI
expServer reg idom =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> pure expHTMLHandler
    :<|> expWSHandler reg idom

-- | Create the web application
expApp :: Registry -> DomInit -> Wai.Application
expApp reg idom = serve (Proxy @ExpAPI) $ expServer reg idom

-- | Start the Warp WEB server to serve the application
runServer :: [Widget] -> DomInit -> IO ()
runServer widgets idom = do
  registry <- atomically $ initRegistry widgets
  Warp.run 8092 $ expApp registry idom
