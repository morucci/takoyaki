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

data Widget = Widget
  { wId :: WidgetId,
    wSwap :: WSwapStrategy,
    wsEvent :: WSEvent -> Maybe WEvent,
    wRender :: WState -> Html (),
    wState :: WState,
    wStateUpdate :: WState -> WEvent -> WState,
    wTrigger :: Maybe (Maybe Trigger)
  }

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

processEventWidget :: Registry -> WEvent -> Widget -> STM (Html ())
processEventWidget reg event widget = do
  let newState = wStateUpdate widget widget.wState event
      newWidget = widget {wState = newState}
  addWidget reg newWidget
  pure $ widgetRender newWidget

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
