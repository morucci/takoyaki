module Takoyaki.Htmx where

import Data.Aeson as Aeson (FromJSON, Value (Object, String), decode, withObject)
import Data.Aeson.Key as Aeson (fromText)
import Data.Aeson.KeyMap as Aeson (lookup)
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Text
import Lucid (Attribute)
import Lucid.Base (makeAttribute)
import qualified Network.WebSockets as WS
import qualified XStatic
import qualified XStatic.Htmx as XStatic
import Prelude

xStaticFiles :: [XStatic.XStaticFile]
xStaticFiles = [XStatic.htmx, XStatic.htmxExtWS]

-- | Attributes definition for Lucid
hxWS, hxSwapOOB, wsConnect, wsSend, hxTrigger, hxTarget, hxVals :: Text -> Attribute
hxWS = makeAttribute "hx-ws"
hxSwapOOB = makeAttribute "hx-swap-oob"
wsConnect = makeAttribute "ws-connect"
wsSend = makeAttribute "ws-send"
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxVals = makeAttribute "hx-vals"

hxExtWS :: Attribute
hxExtWS = makeAttribute "hx-ext" "ws"

data WSwapStrategy = InnerHTML | BeforeBegin

swapToText :: WSwapStrategy -> Text
swapToText sw = case sw of
  InnerHTML -> "innerHTML"
  BeforeBegin -> "beforebegin"

type WidgetId = Text

type TriggerId = Text

type Trigger = Text

data WSEvent = WSEvent
  { wseWidgetId :: WidgetId,
    wseTriggerId :: TriggerId
  }
  deriving (Show)

instance FromJSON WSEvent where
  parseJSON = withObject "" $ \v -> do
    let widgetIdM = case Aeson.lookup (fromText "widgetId") v of
          Just (String widgetId) -> Just widgetId
          _otherwide -> Nothing
        triggerIdM = case Aeson.lookup (fromText "HEADERS") v of
          Just (Object nv) -> do
            case Aeson.lookup (fromText "HX-Trigger") nv of
              Just (String triggerId) -> Just triggerId
              _otherWise -> Nothing
          _otherwise -> Nothing
        eventM = WSEvent <$> widgetIdM <*> triggerIdM
    case eventM of
      Just event -> pure event
      Nothing -> fail "String"

decodeWSEvent :: WS.DataMessage -> Maybe WSEvent
decodeWSEvent (WS.Text dm _) = decode dm
decodeWSEvent _ = Nothing
