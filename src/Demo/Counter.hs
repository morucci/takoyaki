-- |
-- Module      : Exp
-- Description : Some experimentation
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- Add desc
module Demo.Counter where

import Control.Monad.State (MonadState (put), State, get)
import qualified Data.Aeson as Aeson
import Data.Set
import Lucid (Html, ToHtml (toHtml))
import Lucid.Html5
import Takoyaki.Engine
  ( ChildsStore,
    WEvent (WEvent),
    WState,
    Widget (..),
    runServer,
    widgetRenderFromChildsStore,
    withEvent,
  )
import Takoyaki.Htmx
  ( WSEvent (..),
    WSwapStrategy (InnerHTML),
  )
import Prelude

counterW :: Widget
counterW =
  Widget
    { wId = "Counter",
      wSwap = InnerHTML,
      wsEvent,
      wRender = const wRender,
      wState = Just (Aeson.Number 0),
      wStateUpdate,
      wTrigger,
      wChilds = mempty
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTriggerId == "IncButton" = Just $ WEvent "Counter" "IncCounter"
      | e.wseTriggerId == "DecrButton" = Just $ WEvent "Counter" "DecrCounter"
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

counterControlW :: Widget
counterControlW =
  Widget
    { wId = "CounterControl",
      wSwap = InnerHTML,
      wsEvent,
      wRender = const wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger,
      wChilds = mempty
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
      wRender = const wRender,
      wState = Just (Aeson.Number 0),
      wStateUpdate,
      wTrigger,
      wChilds = mempty
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

mainW :: Widget
mainW =
  Widget
    { wId = "mainW",
      wSwap = InnerHTML,
      wsEvent = const Nothing,
      wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger = Nothing,
      wChilds = fromList [counterW, counterControlW, counterDisplayW]
    }
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender rs = pure $ do
      let counter' = widgetRenderFromChildsStore "Counter" rs
          counterCW = widgetRenderFromChildsStore "CounterControl" rs
          counterDW = widgetRenderFromChildsStore "CounterDisplay" rs
      div_ [id_ "my-dom"] $ do
        div_ $ do
          p_ "Counter"
          counter'
        div_ $ do
          p_ "CounterControl + CounterDisplay"
          span_ $ do
            counterDW
            counterCW

run :: IO ()
run = runServer "Takoyaki Counter Demo" widgets mainW
  where
    widgets = [mainW, counterW, counterControlW, counterDisplayW]
