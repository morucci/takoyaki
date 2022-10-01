{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Exp
-- Description : Some experimentation
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- Add desc
module Exp where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as Aeson (FromJSON, Value (Object, String), decode, withObject)
import Data.Aeson.Key as Aeson (fromText)
import Data.Aeson.KeyMap as Aeson (fromList, lookup)
import qualified Data.Aeson.Text as Aeson
import Data.Aeson.Types (FromJSON (parseJSON))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Lucid (Attribute, Html, ToHtml (toHtml), With (with), renderBS)
import Lucid.Base (makeAttribute)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid (HTML)
import Servant.XStatic
import qualified XStatic
import qualified XStatic.Htmx as XStatic
import qualified XStatic.Tailwind as XStatic
import Prelude

-- | Some HTMX attribute definition for Lucid
hxWS, hxSwapOOB :: Text -> Attribute
hxWS = makeAttribute "hx-ws"
hxSwapOOB = makeAttribute "hx-swap-oob"

type ExpAPI =
  "xstatic" :> Raw
    :<|> "exp" :> Get '[HTML] (Html ())
    :<|> "exp" :> "ws" :> WebSocket

expServer :: Server ExpAPI
expServer =
  xstaticServant xStaticFiles
    :<|> pure expHTMLHandler
    :<|> expWSHandler

-- | We need static assets HTMX and Tailwindcss
xStaticFiles :: [XStatic.XStaticFile]
xStaticFiles = [XStatic.htmx, XStatic.htmxExtWS, XStatic.tailwind]

-- | Create the web application
expApp :: Wai.Application
expApp = serve (Proxy @ExpAPI) $ expServer

-- | Start the Warp WEB server to serve the application
runServer :: IO ()
runServer = Warp.run 8091 $ expApp

instance FromJSON WSEvent where
  parseJSON = withObject "" $ \v -> do
    case Aeson.lookup (fromText "widgetId") v of
      Just (String widgetId) -> case Aeson.lookup (fromText "HEADERS") v of
        Just (Object nv) -> do
          case Aeson.lookup (fromText "HX-Trigger") nv of
            Just (String triggerId) -> pure $ WSEvent widgetId triggerId
            _otherWise -> failD v
        _otherwise -> failD v
      _otherwise -> failD v
    where
      failD v = fail $ "Unable to decode payload: " <> show v

data WSwapStrategy = InnerHTML | BeforeBegin

type Id = Text

type Trigger = Text

data WSEvent = WSEvent
  { wseWidgetId :: Id,
    wseTriggerId :: Id
  }
  deriving (Show)

data Widget s e = Widget
  { wId :: Id,
    wSwap :: WSwapStrategy,
    wsEvent :: WSEvent -> Maybe e,
    wRender :: s -> Html (),
    wState :: s,
    wStateUpdate :: s -> e -> s,
    wTrigger :: Maybe (Maybe Trigger)
  }

data CounterEvent = IncCounter | DecrCounter

w1 :: Widget Int CounterEvent
w1 =
  Widget
    { wId = "Counter",
      wSwap = InnerHTML,
      wsEvent,
      wRender,
      wState = 0 :: Int,
      wStateUpdate,
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe CounterEvent
    wsEvent e
      | (wseTriggerId e) == "IncButton" = Just IncCounter
      | (wseTriggerId e) == "DecrButton" = Just DecrCounter
      | otherwise = Nothing
    wRender :: Int -> Html ()
    wRender s = do
      div_ $ do
        span_ $ do
          withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
          withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
          span_ [class_ "mx-2"] $ toHtml $ show s
    wStateUpdate :: Int -> CounterEvent -> Int
    wStateUpdate s e = case e of
      IncCounter -> s + 1
      DecrCounter -> s - 1
    wTrigger = Nothing

-- If trigger not specified then the fallback is the natural event
withEvent :: Id -> Maybe Trigger -> Html () -> Html ()
withEvent eid triggerM elm =
  let elm' = with elm [id_ eid, wsSend ""]
   in case triggerM of
        Just trigger -> with elm' [hxTrigger trigger]
        Nothing -> elm'

type WStore s e = Map.Map Id (Widget s e)

type Registry s e = TVar (WStore s e)

initWStore :: STM (TVar (WStore s e))
initWStore = newTVar Map.empty

addWidget :: TVar (WStore s e) -> Widget s e -> STM ()
addWidget st w = modifyTVar st $ Map.insert (wId w) w

getWidgetIds :: Registry s e -> STM [Id]
getWidgetIds reg = do
  st <- readTVar reg
  pure $ Map.keys st

renderWidget :: TVar (WStore s e) -> Id -> STM (Html ())
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

widgetHandleEvent :: WSEvent -> Widget s e -> Maybe (Widget s e)
widgetHandleEvent wsevent widget = do
  case (wsEvent widget wsevent) of
    Just wEvent -> do
      let newState = wStateUpdate widget (wState widget) wEvent
          newWidget = widget {wState = newState}
      pure newWidget
    Nothing -> Nothing

decodeEvent :: WS.DataMessage -> Maybe WSEvent
decodeEvent (WS.Text dm _) = decode dm
decodeEvent _ = Nothing

expWSHandler :: WS.Connection -> Handler ()
expWSHandler conn = do
  registry <- liftIO $ atomically initWStore
  liftIO $ atomically $ addWidget registry w1
  liftIO $ WS.withPingThread conn 5 (pure ()) $ do
    putStrLn [i|New connection ...|]
    -- Send the rest of WEB UI to the client
    dom <- atomically $ getDom registry
    WS.sendTextData conn $ renderBS $ div_ [id_ "init"] dom
    handleClient registry
  where
    handleClient registry = do
      msg <- WS.receiveDataMessage conn
      handleDataMessage registry msg
      handleClient registry
    getDom :: Registry s e -> STM (Html ())
    getDom registry = do
      counterW <- renderWidget registry "Counter"
      pure $ div_ [id_ "my-dom"] $ do
        counterW
    handleDataMessage :: Registry s e -> WS.DataMessage -> IO ()
    handleDataMessage registry msg = do
      let eventM = decodeEvent msg
      putStrLn $ "payload received: " <> show eventM
      case eventM of
        Nothing -> pure ()
        Just event -> do
          bss <- atomically $ processEventWidgets registry event
          mapM_ (WS.sendTextData conn . renderBS) bss

expHTMLHandler :: Html ()
expHTMLHandler = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Experiment"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts xStaticFiles
      script_ [iii||]
    body_ $ do
      div_ [class_ "container mx-auto", hxExtWS, wsConnect "/exp/ws"] $
        div_ [id_ "init"] ""

hxExtWS :: Attribute
hxExtWS = makeAttribute "hx-ext" "ws"

wsConnect :: Text -> Attribute
wsConnect = makeAttribute "ws-connect"

wsSend :: Text -> Attribute
wsSend = makeAttribute "ws-send"

hxTrigger :: Trigger -> Attribute
hxTrigger = makeAttribute "hx-trigger"

hxVals :: Text -> Attribute
hxVals = makeAttribute "hx-vals"
