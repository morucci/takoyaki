{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

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
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i, iii)
import Engine
  ( Registry,
    WEvent (WEvent),
    WState,
    Widget (..),
    addWidget,
    getWidgetsEvents,
    initWStore,
    processEventWidgets,
    renderWidget,
    withEvent,
  )
import Htmx
  ( WSEvent (..),
    WSwapStrategy (InnerHTML),
    WidgetId,
    decodeEvent,
    hxExtWS,
    wsConnect,
    xStaticFiles,
  )
import Lucid (Html, ToHtml (toHtml), renderBS)
import Lucid.Html5
import Lucid.XStatic (xstaticScripts)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid (HTML)
import Servant.XStatic
import qualified XStatic.Tailwind as XStatic
import Prelude

type ExpAPI =
  "xstatic" :> Raw
    :<|> "exp" :> Get '[HTML] (Html ())
    :<|> "exp" :> "ws" :> WebSocket

expServer :: Server ExpAPI
expServer =
  xstaticServant (xStaticFiles <> [XStatic.tailwind])
    :<|> pure expHTMLHandler
    :<|> expWSHandler [w1]

-- | Create the web application
expApp :: Wai.Application
expApp = serve (Proxy @ExpAPI) $ expServer

-- | Start the Warp WEB server to serve the application
runServer :: IO ()
runServer = Warp.run 8092 $ expApp

w1 :: Widget
w1 =
  Widget
    { wId = "Counter",
      wSwap = InnerHTML,
      wsEvent,
      wRender,
      wState = Aeson.Number 0,
      wStateUpdate,
      wTrigger
    }
  where
    wsEvent :: WidgetId -> WSEvent -> Maybe WEvent
    wsEvent wId e
      | (wseWidgetId e) == wId && (wseTriggerId e) == "IncButton" = Just $ WEvent "IncCounter"
      | (wseWidgetId e) == wId && (wseTriggerId e) == "DecrButton" = Just $ WEvent "DecrCounter"
      | otherwise = Nothing
    wRender :: WState -> Html ()
    wRender (Aeson.Number i') = do
      div_ $ do
        span_ $ do
          withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
          withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
          span_ [class_ "mx-2"] $ toHtml $ show i'
    wRender _ = error "Unexpected state type"
    wStateUpdate :: WState -> WEvent -> WState
    wStateUpdate s@(Aeson.Number i') e = case e of
      WEvent "IncCounter" -> Aeson.Number (i' + 1)
      WEvent "DecrCounter" -> Aeson.Number (i' - 1)
      _otherwise -> s
    wStateUpdate _ _ = error "Unexpected state type"
    wTrigger = Nothing

-- w1' :: Widget Int Counter2Event
-- w1' =
--   Widget
--     { wId = "Counter2",
--       wSwap = InnerHTML,
--       wsEvent,
--       wRender,
--       wState = 0 :: Int,
--       wStateUpdate,
--       wTrigger
--     }
--   where
--     wsEvent :: WSEvent -> Maybe Counter2Event
--     wsEvent e
--       | (wseTriggerId e) == "IncButton" = Just Inc2Counter
--       | (wseTriggerId e) == "DecrButton" = Just Decr2Counter
--       | otherwise = Nothing
--     wRender :: Int -> Html ()
--     wRender s = do
--       div_ $ do
--         span_ $ do
--           withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
--           withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
--           span_ [class_ "mx-2"] $ toHtml $ show s
--     wStateUpdate :: Int -> Counter2Event -> Int
--     wStateUpdate s e = case e of
--       Inc2Counter -> s + 1
--       Decr2Counter -> s - 1
--     wTrigger = Nothing

-- w2 :: Widget Bool SwitchEvent
-- w2 =
--   Widget
--     { wId = "Switch",
--       wSwap = InnerHTML,
--       wsEvent,
--       wRender,
--       wState = False :: Bool,
--       wStateUpdate,
--       wTrigger
--     }
--   where
--     wsEvent :: WSEvent -> Maybe SwitchEvent
--     wsEvent e
--       | (wseTriggerId e) == "SwitchButton" = Just Switch
--       | otherwise = Nothing
--     wRender :: Bool -> Html ()
--     wRender s = do
--       div_ $ do
--         span_ $ do
--           withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
--           withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
--           span_ [class_ "mx-2"] $ toHtml $ show s
--     wStateUpdate :: Bool -> SwitchEvent -> Bool
--     wStateUpdate s e = case e of
--       Switch -> not s
--     wTrigger = Nothing

expWSHandler :: [Widget] -> WS.Connection -> Handler ()
expWSHandler widgets conn = do
  queue <- liftIO . atomically $ newTBQueue 10
  liftIO $ WS.withPingThread conn 5 (pure ()) $ do
    registry <- atomically $ do
      reg <- initWStore
      mapM_ (addWidget reg) widgets
      pure reg
    putStrLn [i|New connection ...|]
    dom <- atomically $ getDom registry
    WS.sendTextData conn $ renderBS $ div_ [id_ "init"] dom
    handleClient registry queue
  where
    handleClient registry queue = do
      msg <- WS.receiveDataMessage conn
      handleDataMessage registry queue msg
      handleClient registry queue
    getDom :: Registry -> STM (Html ())
    getDom registry = do
      counterW <- renderWidget registry "Counter"
      pure $ div_ [id_ "my-dom"] $ do
        counterW
    -- handleDataMessage :: IsWEvent e => Registry s e -> WS.DataMessage -> IO ()
    handleDataMessage registry queue msg = do
      let eventM = decodeEvent msg
      putStrLn $ "payload received: " <> show eventM
      case eventM of
        Nothing -> pure ()
        Just event -> do
          atomically $ do
            widgetEvents <- getWidgetsEvents registry event
            mapM_ (writeTBQueue queue) widgetEvents
          bss <- atomically $ processEventWidgets registry event
          mapM_ (WS.sendTextData conn . renderBS) bss

expHTMLHandler :: Html ()
expHTMLHandler = do
  doctypehtml_ $ do
    head_ $ do
      title_ "Experiment"
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts $ xStaticFiles <> [XStatic.tailwind]
      script_ [iii||]
    body_ $ do
      div_ [class_ "container mx-auto", hxExtWS, wsConnect "/exp/ws"] $
        div_ [id_ "init"] ""
