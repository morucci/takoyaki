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
import Control.Monad.State (MonadState (put), State, get)
import qualified Data.Aeson as Aeson
import Lucid (Html, ToHtml (toHtml))
import Lucid.Html5
import Takoyaki.Engine
  ( Registry,
    WEvent (WEvent),
    WState,
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
      wState = Just (Aeson.Number 0),
      wStateUpdate,
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTriggerId == "IncButton" = Just $ WEvent wId "IncCounter"
      | e.wseTriggerId == "DecrButton" = Just $ WEvent wId "DecrCounter"
      | otherwise = Nothing
    wRender :: State (Maybe WState) (Html ())
    wRender = do
      ws <- get
      case ws of
        Just (Aeson.Number i') -> pure $ div_ $ do
          span_ $ do
            withEvent "IncButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
            withEvent "DecrButton" Nothing $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
            span_ [class_ "mx-2"] $ toHtml $ show i'
        _otherwise -> error "Unexpected widget state"
    wStateUpdate :: WEvent -> State (Maybe WState) ()
    wStateUpdate e = do
      ws <- get
      case (ws, e) of
        (Just (Aeson.Number i'), WEvent _ "IncCounter") -> put . Just $ Aeson.Number (i' + 1)
        (Just (Aeson.Number i'), WEvent _ "DecrCounter") -> put . Just $ Aeson.Number (i' - 1)
        _otherwise -> put ws
      pure ()
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
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTriggerId == "IncButton" = Just $ WEvent "CounterDisplay" "IncCounter"
      | e.wseTriggerId == "DecrButton" = Just $ WEvent "CounterDisplay" "DecrCounter"
      | otherwise = Nothing
    wRender :: State (Maybe WState) (Html ())
    wRender = do
      pure $ div_ $ do
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
      wState = Just (Aeson.Number 0),
      wStateUpdate,
      wTrigger
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent _ = Nothing
    wRender :: State (Maybe WState) (Html ())
    wRender = do
      ws <- get
      case ws of
        Just (Aeson.Number i') -> pure $ div_ $ span_ [class_ "mx-2"] $ toHtml $ show i'
        _otherwise -> error "Unexpected widget state"
    wStateUpdate :: WEvent -> State (Maybe WState) ()
    wStateUpdate e = do
      ws <- get
      case (ws, e) of
        (Just (Aeson.Number i'), WEvent _ "IncCounter") -> put . Just $ Aeson.Number (i' + 1)
        (Just (Aeson.Number i'), WEvent _ "DecrCounter") -> put . Just $ Aeson.Number (i' - 1)
        _otherwise -> put ws
      pure ()
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
