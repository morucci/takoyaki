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
module Demo.Counter where

import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import Lucid (Html, ToHtml (toHtml))
import Lucid.Html5
import Takoyaki.Engine
  ( Registry,
    WEvent (WEvent),
    WState,
    WStateManager (WStateManager),
    Widget (..),
    renderWidget,
    runServer,
    withEvent,
  )
import Takoyaki.Htmx
  ( WSEvent (..),
    WSwapStrategy (InnerHTML),
    WidgetId,
  )
import Prelude

getCounterW :: WidgetId -> Widget
getCounterW wId =
  Widget
    { wId,
      wSwap = InnerHTML,
      wsEvent,
      wRender,
      wStateM = Just (WStateManager (Aeson.Number 0) wStateUpdate),
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTriggerId == "IncButton" = Just $ WEvent wId "IncCounter"
      | e.wseTriggerId == "DecrButton" = Just $ WEvent wId "DecrCounter"
      | otherwise = Nothing
    wRender :: Maybe WState -> Html ()
    wRender (Just (Aeson.Number i')) = do
      div_ $ do
        span_ $ do
          withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
          withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
          span_ [class_ "mx-2"] $ toHtml $ show i'
    wRender _ = error "Unexpected state type"
    wStateUpdate :: WState -> WEvent -> WState
    wStateUpdate s@(Aeson.Number i') e = case e of
      WEvent _ "IncCounter" -> Aeson.Number (i' + 1)
      WEvent _ "DecrCounter" -> Aeson.Number (i' - 1)
      _otherwise -> s
    wStateUpdate _ _ = error "Unexpected state type"
    wTrigger = Nothing

counter1W, counter2W :: Widget
counter1W = getCounterW "Counter1"
counter2W = getCounterW "Counter2"

counterControlW :: Widget
counterControlW =
  Widget
    { wId = "CounterControl",
      wSwap = InnerHTML,
      wsEvent,
      wRender,
      wStateM = Nothing,
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTriggerId == "IncButton" = Just $ WEvent "CounterDisplay" "IncCounter"
      | e.wseTriggerId == "DecrButton" = Just $ WEvent "CounterDisplay" "DecrCounter"
      | otherwise = Nothing
    wRender :: Maybe WState -> Html ()
    wRender _ = do
      div_ $ do
        span_ $ do
          withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
          withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
    wTrigger = Nothing

counterDisplayW :: Widget
counterDisplayW =
  Widget
    { wId = "CounterDisplay",
      wSwap = InnerHTML,
      wsEvent,
      wRender,
      wStateM = Just (WStateManager (Aeson.Number 0) wStateUpdate),
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent _ = Nothing
    wRender :: Maybe WState -> Html ()
    wRender (Just (Aeson.Number i')) = do
      div_ $ do
        span_ [class_ "mx-2"] $ toHtml $ show i'
    wRender _ = error "Unexpected state type"
    wStateUpdate :: WState -> WEvent -> WState
    wStateUpdate s@(Aeson.Number i') e = case e of
      WEvent _ "IncCounter" -> Aeson.Number (i' + 1)
      WEvent _ "DecrCounter" -> Aeson.Number (i' - 1)
      _otherwise -> s
    wStateUpdate _ _ = error "Unexpected state type"
    wTrigger = Nothing

run :: IO ()
run = runServer widget initDom
  where
    widget = [counter1W, counter2W, counterControlW, counterDisplayW]
    initDom :: Registry -> STM (Html ())
    initDom registry = do
      counter1W' <- renderWidget registry "Counter1"
      counter2W' <- renderWidget registry "Counter2"
      counterCW <- renderWidget registry "CounterControl"
      counterDW <- renderWidget registry "CounterDisplay"
      pure $ div_ [id_ "my-dom"] $ do
        div_ $ do
          p_ "Counter1"
          counter1W'
        div_ $ do
          p_ "Counter2"
          counter2W'
        div_ $ do
          p_ "CounterControl + CounterDisplay"
          span_ $ do
            counterDW
            counterCW
