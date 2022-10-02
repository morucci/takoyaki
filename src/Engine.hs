module Engine where

import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Htmx (Trigger, WSEvent, WSwapStrategy, WidgetId, hxSwapOOB, hxTrigger, hxVals, swapToText, wsSend)
import Lucid (Html, with)
import Lucid.Html5
import Prelude

data WEvent = WEvent Text

type WState = Aeson.Value

type WStore = Map.Map WidgetId Widget

type Registry = TVar WStore

data Widget = Widget
  { wId :: WidgetId,
    wSwap :: WSwapStrategy,
    wsEvent :: WidgetId -> WSEvent -> Maybe WEvent,
    wRender :: WState -> Html (),
    wState :: WState,
    wStateUpdate :: WState -> WEvent -> WState,
    wTrigger :: Maybe (Maybe Trigger)
  }

initWStore :: STM Registry
initWStore = newTVar Map.empty

addWidget :: Registry -> Widget -> STM ()
addWidget st w = modifyTVar st $ Map.insert w.wId w

getWidgetIds :: Registry -> STM [WidgetId]
getWidgetIds reg = Map.keys <$> readTVar reg

getWidgets :: Registry -> STM [Widget]
getWidgets reg = Map.elems <$> readTVar reg

getWidgetsEvents :: Registry -> WSEvent -> STM [WEvent]
getWidgetsEvents reg event = do
  widgets <- getWidgets reg
  catMaybes <$> mapM go widgets
  where
    go w = pure $ wsEvent w w.wId event

renderWidget :: Registry -> WidgetId -> STM (Html ())
renderWidget reg wid = do
  st <- readTVar reg
  case Map.lookup wid st of
    Just w -> pure $ widgetRender w
    Nothing -> pure mempty

processEventWidget :: Registry -> WSEvent -> WidgetId -> STM (Maybe (Html ()))
processEventWidget reg event wid = do
  st <- readTVar reg
  let newWidgetM = case Map.lookup wid st of
        Just w -> widgetHandleEvent event w
        Nothing -> Nothing
  case newWidgetM of
    Just new -> do
      addWidget reg new
      pure $ Just $ widgetRender new
    Nothing -> pure Nothing

processEventWidgets :: Registry -> WSEvent -> STM [Html ()]
processEventWidgets reg event = do
  ids <- getWidgetIds reg
  catMaybes <$> (mapM (processEventWidget reg event) ids)

widgetHandleEvent :: WSEvent -> Widget -> Maybe Widget
widgetHandleEvent wsevent widget = do
  case (wsEvent widget widget.wId wsevent) of
    Just wEvent -> do
      let newState = wStateUpdate widget widget.wState wEvent
          newWidget = widget {wState = newState}
      pure newWidget
    Nothing -> Nothing

widgetRender :: Widget -> Html ()
widgetRender w = with elm [wIdVal, hxSwapOOB . swapToText $ wSwap w]
  where
    elm = case wTrigger w of
      Nothing -> with baseElm [id_ w.wId]
      Just triggerM -> withEvent w.wId triggerM baseElm
    baseElm = div_ $ wRender w w.wState
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
