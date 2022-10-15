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
import Prelude

counterW :: Widget
counterW =
  (mkWidget "Counter")
    { wsEvent,
      wRender = const wRender,
      wState = Just (Aeson.Number 0),
      wStateUpdate
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTId == "IncButton" = Just $ WEvent "Counter" "IncCounter" Aeson.Null
      | e.wseTId == "DecrButton" = Just $ WEvent "Counter" "DecrCounter" Aeson.Null
      | otherwise = Nothing
    wRender :: State (Maybe WState) (Html ())
    wRender = do
      ws <- get
      case ws of
        Just (Aeson.Number i') -> pure $ div_ $ do
          span_ $ do
            withEvent "IncButton" Nothing [] $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
            withEvent "DecrButton" Nothing [] $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"
            span_ [class_ "mx-2"] $ toHtml $ show i'
        _otherwise -> error "Unexpected widget state"
    wStateUpdate :: WEvent -> State (Maybe WState) ()
    wStateUpdate e = do
      ws <- get
      case (ws, e) of
        (Just (Aeson.Number i'), WEvent _ "IncCounter" _) -> put . Just $ Aeson.Number (i' + 1)
        (Just (Aeson.Number i'), WEvent _ "DecrCounter" _) -> put . Just $ Aeson.Number (i' - 1)
        _otherwise -> put ws
      pure ()

counterControlW :: Widget
counterControlW =
  (mkWidget "CounterControl")
    { wsEvent,
      wRender = const wRender
    }
  where
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTId == "IncButton" = Just $ WEvent "CounterDisplay" "IncCounter" Aeson.Null
      | e.wseTId == "DecrButton" = Just $ WEvent "CounterDisplay" "DecrCounter" Aeson.Null
      | otherwise = Nothing
    wRender :: State (Maybe WState) (Html ())
    wRender = do
      pure $ div_ $ do
        span_ $ do
          withEvent "IncButton" Nothing [] $ button_ [class_ "bg-black text-white mx-2 px-2"] "Inc"
          withEvent "DecrButton" Nothing [] $ button_ [class_ "bg-black text-white mx-2 px-2"] "Decr"

counterDisplayW :: Widget
counterDisplayW =
  (mkWidget "CounterDisplay")
    { wRender = const wRender,
      wState = Just (Aeson.Number 0),
      wStateUpdate
    }
  where
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
        (Just (Aeson.Number i'), WEvent _ "IncCounter" _) -> put . Just $ Aeson.Number (i' + 1)
        (Just (Aeson.Number i'), WEvent _ "DecrCounter" _) -> put . Just $ Aeson.Number (i' - 1)
        _otherwise -> put ws
      pure ()

mainW :: Widget
mainW =
  (mkWidget "mainW")
    { wRender,
      wChilds = fromList [counterW, counterControlW, counterDisplayW]
    }
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender cs = pure $ do
      let counter' = renderW cs "Counter"
          counterCW = renderW cs "CounterControl"
          counterDW = renderW cs "CounterDisplay"
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
