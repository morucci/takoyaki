{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Demo.MineSweeper where

import Control.Monad.State
import qualified Data.Map as Map
import Lucid
import Takoyaki.Engine
import Prelude

data MSState = MSState

data MSEvent = NewBoard deriving (Show)

mineSweeperApp :: App MSState MSEvent
mineSweeperApp =
  App
    { appName = "MineSweeper",
      appWSEvent = wSEvent,
      appState = MSState,
      appRender = renderApp,
      appHandleEvent = handleEvent
    }

wSEvent :: WSEvent -> Maybe ([IO MSEvent])
wSEvent (WSEvent wseName _ wseData) = case wseName of
  _ -> Nothing
  where
    _getData = flip Map.lookup wseData

handleEvent :: MSEvent -> State MSState (Html ())
handleEvent _ev = do
  _appState <- get
  pure $ pure ()

renderApp :: State MSState (Html ())
renderApp = do
  _appState <- get
  pure $ div_ [class_ "bg-gray-200"] $ do
    h1_ "MineSweeper"

run :: IO ()
run = runServer mineSweeperApp
