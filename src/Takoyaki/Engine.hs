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
import Data.Maybe (catMaybes)
import Data.Set as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Lucid (Html, ToHtml (toHtml), renderBS, with)
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

type ChildsStore = Map.Map WidgetId (Html ())

data Widget = Widget
  { wId :: WidgetId,
    wSwap :: WSwapStrategy,
    wsEvent :: WSEvent -> Maybe WEvent,
    wRender :: ChildsStore -> State (Maybe WState) (Html ()),
    wState :: Maybe WState,
    wStateUpdate :: WEvent -> State (Maybe WState) (),
    wTrigger :: Maybe (Maybe Trigger),
    wChilds :: Set Widget
  }

instance Show Widget where
  show w = "Widget: " <> show w.wId <> " - State: " <> show w.wState

instance Eq Widget where
  (==) a b = a.wId == b.wId

instance Ord Widget where
  (<=) a b = a.wId <= b.wId

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

mkChildsStore :: Registry -> Set Widget -> STM ChildsStore
mkChildsStore reg widgets = do
  rs <- mapM go $ toList widgets
  pure . Map.fromList $ catMaybes rs
  where
    go :: Widget -> STM (Maybe (WidgetId, Html ()))
    go widget = do
      widgetFromRegistryM <- getWidget reg widget.wId
      case widgetFromRegistryM of
        Just w -> do
          rs' <- mkChildsStore reg w.wChilds
          pure $ Just (w.wId, widgetRender rs' widget)
        Nothing -> pure Nothing

processEventWidget :: Registry -> WEvent -> Widget -> STM Widget
processEventWidget reg event w = do
  let newState = execState (w.wStateUpdate event) w.wState
      newWidget = w {wState = newState}
  addWidget reg newWidget
  pure newWidget

widgetRenderFromChildsStore :: WidgetId -> ChildsStore -> Html ()
widgetRenderFromChildsStore w rs = case Map.lookup w rs of
  Just r -> r
  Nothing -> pure ()

widgetRender :: ChildsStore -> Widget -> Html ()
widgetRender rs w = with elm [wIdVal, hxSwapOOB . swapToText $ wSwap w]
  where
    elm = case wTrigger w of
      Nothing -> with baseElm [id_ w.wId]
      Just triggerM -> withEvent w.wId triggerM baseElm
    baseElm = div_ $ evalState (w.wRender rs) w.wState
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

connectionHandler :: Registry -> (Registry -> STM (Html ())) -> WS.Connection -> Handler ()
connectionHandler registry initDom conn = do
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
          rs <- atomically $ mkChildsStore registry widgetToRender.wChilds
          WS.sendTextData conn $ renderBS $ widgetRender rs widgetToRender
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

appServer :: Text -> Registry -> DomInit -> Server API
appServer title reg idom =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> pure (bootHandler title)
    :<|> connectionHandler reg idom

-- | Create the web application
app :: Text -> Registry -> DomInit -> Wai.Application
app title reg idom = serve (Proxy @API) $ appServer title reg idom

-- | Start the Warp WEB server to serve the application
runServer :: Text -> [Widget] -> DomInit -> IO ()
runServer title widgets idom = do
  registry <- atomically $ initRegistry widgets
  Warp.run 8092 $ app title registry idom
