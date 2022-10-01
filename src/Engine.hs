module Engine where

import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar)
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text.Lazy (toStrict)
import Htmx (Id, Trigger, WSEvent, WSwapStrategy, hxTrigger, hxVals, wsSend)
import Lucid (Html, with)
import Lucid.Html5
import Prelude

type WStore s e = Map.Map Id (Widget s e)

type Registry s e = TVar (WStore s e)

data Widget s e = Widget
  { wId :: Id,
    wSwap :: WSwapStrategy,
    wsEvent :: WSEvent -> Maybe e,
    wRender :: s -> Html (),
    wState :: s,
    wStateUpdate :: s -> e -> s,
    wTrigger :: Maybe (Maybe Trigger)
  }

initWStore :: STM (Registry s e)
initWStore = newTVar Map.empty

addWidget :: Registry s e -> Widget s e -> STM ()
addWidget st w = modifyTVar st $ Map.insert (wId w) w

getWidgetIds :: Registry s e -> STM [Id]
getWidgetIds reg = Map.keys <$> readTVar reg

renderWidget :: Registry s e -> Id -> STM (Html ())
renderWidget reg wid = do
  st <- readTVar reg
  case Map.lookup wid st of
    Just w -> pure $ widgetRender w
    Nothing -> pure mempty

processEventWidget :: Registry s e -> WSEvent -> Id -> STM (Maybe (Html ()))
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

processEventWidgets :: Registry s e -> WSEvent -> STM [Html ()]
processEventWidgets reg event = do
  ids <- getWidgetIds reg
  catMaybes <$> (mapM (processEventWidget reg event) ids)

widgetRender :: Widget s e -> Html ()
widgetRender w = with elm [wIdVal]
  where
    elm = case wTrigger w of
      Nothing -> with baseElm [id_ (wId w)]
      Just triggerM -> withEvent (wId w) triggerM baseElm
    baseElm = div_ (wRender w $ wState w)
    wIdVal =
      hxVals
        . toStrict
        . Aeson.encodeToLazyText
        $ Aeson.fromList [("widgetId", wId w)]

-- If trigger not specified then the fallback is the natural event
withEvent :: Id -> Maybe Trigger -> Html () -> Html ()
withEvent eid triggerM elm =
  let elm' = with elm [id_ eid, wsSend ""]
   in case triggerM of
        Just trigger -> with elm' [hxTrigger trigger]
        Nothing -> elm'

widgetHandleEvent :: WSEvent -> Widget s e -> Maybe (Widget s e)
widgetHandleEvent wsevent widget = do
  case (wsEvent widget wsevent) of
    Just wEvent -> do
      let newState = wStateUpdate widget (wState widget) wEvent
          newWidget = widget {wState = newState}
      pure newWidget
    Nothing -> Nothing
