module Engine where

import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Htmx (Trigger, WSEvent (..), WSwapStrategy, WidgetId, hxSwapOOB, hxTrigger, hxVals, swapToText, wsSend)
import Lucid (Html, with)
import Lucid.Html5
import Prelude

data WEvent = WEvent
  { eTarget :: WidgetId,
    eType :: Text
  }
  deriving (Show)

type WState = Aeson.Value

type WStore = Map.Map WidgetId Widget

type Registry = TVar WStore

data WStateManager = WStateManager
  { wState :: WState,
    wStateUpdate :: WState -> WEvent -> WState
  }

data Widget = Widget
  { wId :: WidgetId,
    wSwap :: WSwapStrategy,
    wsEvent :: WSEvent -> Maybe WEvent,
    wRender :: Maybe WState -> Html (),
    wStateM :: Maybe WStateManager,
    wTrigger :: Maybe (Maybe Trigger)
  }

instance Show WStateManager where
  show wsm = show wsm.wState

instance Show Widget where
  show w = "Widget: " <> show w.wId <> " - State: " <> show w.wStateM

initWStore :: STM Registry
initWStore = newTVar Map.empty

addWidget :: Registry -> Widget -> STM ()
addWidget st w = modifyTVar st $ Map.insert w.wId w

getWidget :: Registry -> WidgetId -> STM (Maybe Widget)
getWidget reg wid = do
  st <- readTVar reg
  pure $ Map.lookup wid st

renderWidget :: Registry -> WidgetId -> STM (Html ())
renderWidget reg wid = do
  st <- readTVar reg
  case Map.lookup wid st of
    Just w -> pure $ widgetRender w
    Nothing -> pure mempty

processEventWidget :: Registry -> WEvent -> Widget -> STM Widget
processEventWidget reg event widget = do
  case widget.wStateM of
    Just m@(WStateManager wState wStateUpdate) -> do
      let newState = wStateUpdate wState event
          newWidget = widget {wStateM = Just m {wState = newState}}
      addWidget reg newWidget
      pure newWidget
    Nothing -> pure widget

widgetRender :: Widget -> Html ()
widgetRender w = with elm [wIdVal, hxSwapOOB . swapToText $ wSwap w]
  where
    elm = case wTrigger w of
      Nothing -> with baseElm [id_ w.wId]
      Just triggerM -> withEvent w.wId triggerM baseElm
    baseElm = div_ $ wRender w state
    state = case w.wStateM of
      Just (WStateManager wState _) -> Just wState
      Nothing -> Nothing
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
