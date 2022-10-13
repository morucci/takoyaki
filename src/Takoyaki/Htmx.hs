module Takoyaki.Htmx where

import Data.Aeson as Aeson (FromJSON, Value (Object, String), decode, (.:))
import Data.Aeson.Key as Aeson (toText)
import Data.Aeson.KeyMap as Aeson (filterWithKey, map, toMapText)
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.ByteString.Lazy (ByteString)
import Data.Map
import Data.String.Interpolate (i)
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

decodeWSEvent :: WS.DataMessage -> Maybe WSEvent
decodeWSEvent (WS.Text dm _) = decode dm
decodeWSEvent _ = Nothing

tevent :: ByteString
tevent = [i|{"task-name":"","widgetId":"TodoInputFormW","HEADERS":{"HX-Request":"true","HX-Trigger":"InputFormSubmitted","HX-Trigger-Name":"todo-input","HX-Target":null,"HX-Current-URL":"http://127.0.0.1:8092/"}}|]

data WSEvent = WSEvent
  { wseWId :: WidgetId,
    wseTId :: TriggerId,
    wseHeaders :: Map Text (Maybe Text),
    wseData :: Map Text (Maybe Text)
  }
  deriving (Show)

instance FromJSON WSEvent where
  parseJSON (Object v) = do
    widgetId <- v .: "widgetId"
    headers <- v .: "HEADERS"
    triggerId <- extractTriggerId headers
    pure $ WSEvent widgetId triggerId headers extraKeys
    where
      extraKeys = do
        let filtered =
              Aeson.filterWithKey
                (\k _ -> not $ Aeson.toText k == "HEADERS" || Aeson.toText k == "widgetId")
                v
        Aeson.toMapText $
          Aeson.map
            ( \nv -> case nv of
                (String s) -> Just s
                _ -> Nothing
            )
            filtered
      extractTriggerId headers = case Data.Map.lookup "HX-Trigger" headers of
        Just (Just tid) -> pure tid
        _ -> fail "Unable to extract HX-Trigger from HEADERS"
  parseJSON other = fail $ "Unable to parse WSEvent: " <> show other
