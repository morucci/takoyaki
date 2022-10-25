{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Demo.MineSweeper where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Lucid
import System.Random (randomRIO)
import Takoyaki.Engine
import Takoyaki.Htmx
import Text.Printf (printf)
import Text.Read (readMaybe)
import Witch
import Prelude

data MSState = MSState
  { board :: MSBoard,
    state :: MSGameState
  }
  deriving (Show)

data MSGameState
  = Play UTCTime
  | Win
  | Gameover
  | Wait
  deriving (Show)

data MSCellContent
  = Mine
  | Blank Int
  deriving (Show)

data MSCellStatus = Open | Hidden deriving (Show)

data MSCell = MSCell
  { cellContent :: MSCellContent,
    cellStatus :: MSCellStatus
  }
  deriving (Show)

data MSCellCoord = MSCellCoord
  { cx :: Int,
    cy :: Int
  }
  deriving (Show, Eq, Ord, Generic)

type MSBoard = Map.Map MSCellCoord MSCell

data MSEvent
  = NewGame MSBoard
  | OpenCell MSCellCoord UTCTime
  | UpdateTimer Float
  deriving (Show)

initBoard :: IO MSBoard
initBoard = do
  let cellsCoords = [MSCellCoord x y | x <- [0 .. 9], y <- [0 .. 9]]
      blankBoard = Map.fromList $ map (\coord -> (coord, MSCell (Blank 0) Hidden)) cellsCoords
  minesCoords <- getMinesCoords cellsCoords []
  pure $ setBoard blankBoard minesCoords
  where
    getMinesCoords :: [MSCellCoord] -> [MSCellCoord] -> IO [MSCellCoord]
    getMinesCoords availableCellsCords minesCoords = do
      if length minesCoords == 9
        then pure minesCoords
        else do
          selectedIndex <- randomRIO (0, length availableCellsCords - 1)
          let selectedCoord = availableCellsCords !! selectedIndex
              remainingCellsCords = filter (\cell -> not $ cell == selectedCoord) availableCellsCords
          getMinesCoords remainingCellsCords (minesCoords <> [selectedCoord])
    setBoard :: MSBoard -> [MSCellCoord] -> MSBoard
    setBoard board minesCoords =
      let adjCellds = concatMap getAdjCellCoords minesCoords
          board' = installAdjCells board adjCellds
       in installMines board' minesCoords
      where
        installMines :: MSBoard -> [MSCellCoord] -> MSBoard
        installMines b cs = case cs of
          [] -> b
          [x] -> Map.insert x (MSCell Mine Hidden) b
          (x : xs) -> installMines (Map.insert x (MSCell Mine Hidden) b) (xs)
        installAdjCells :: MSBoard -> [MSCellCoord] -> MSBoard
        installAdjCells b cs = case cs of
          [] -> b
          [x] -> installAdjCell b x
          (x : xs) -> installAdjCells (installAdjCell b x) xs
        installAdjCell :: MSBoard -> MSCellCoord -> MSBoard
        installAdjCell b c =
          Map.insertWith
            ( \_ oldv ->
                case oldv of
                  MSCell (Blank v) s -> MSCell (Blank (v + 1)) s
                  other -> other
            )
            c
            (MSCell (Blank 1) Hidden)
            b

getAdjCellCoords :: MSCellCoord -> [MSCellCoord]
getAdjCellCoords MSCellCoord {..} =
  let isInBoard (MSCellCoord cx' cy') = cx' >= 0 && cx' <= 9 && cy' >= 0 && cy' <= 9
   in filter
        isInBoard
        [ MSCellCoord (cx - 1) (cy - 1),
          MSCellCoord (cx - 1) (cy),
          MSCellCoord (cx - 1) (cy + 1),
          MSCellCoord (cx + 1) (cy - 1),
          MSCellCoord (cx + 1) (cy),
          MSCellCoord (cx + 1) (cy + 1),
          MSCellCoord (cx) (cy - 1),
          MSCellCoord (cx) (cy + 1)
        ]

openCell :: MSCellCoord -> MSBoard -> MSBoard
openCell cellCoord board = Map.update func cellCoord board
  where
    func :: MSCell -> Maybe MSCell
    func (MSCell content _) = Just $ MSCell content Open

getCell :: MSCellCoord -> MSBoard -> Maybe MSCell
getCell = Map.lookup

isBlank0Cell :: MSCellCoord -> MSBoard -> Bool
isBlank0Cell cellCoord board = case getCell cellCoord board of
  Just (MSCell (Blank 0) _) -> True
  _ -> False

isHiddenCell :: MSCellCoord -> MSBoard -> Bool
isHiddenCell cellCoord board = case getCell cellCoord board of
  Just (MSCell _ Hidden) -> True
  _ -> False

isMineCell :: MSCellCoord -> MSBoard -> Bool
isMineCell cellCoord board = case getCell cellCoord board of
  Just (MSCell Mine _) -> True
  _ -> False

countHiddenBlank :: MSBoard -> Int
countHiddenBlank board = length (filter keepHiddenBlank (Map.elems board))
  where
    keepHiddenBlank (MSCell (Blank _) Hidden) = True
    keepHiddenBlank _ = False

openAdjBlank0Cells :: MSCellCoord -> MSBoard -> MSBoard
openAdjBlank0Cells cellCoord board =
  if isBlank0Cell cellCoord board
    then openCells (getAdjCellCoords cellCoord) board
    else board
  where
    openCells :: [MSCellCoord] -> MSBoard -> MSBoard
    openCells cellsCoords b = case cellsCoords of
      [] -> b
      [x] -> openCell' x b
      (x : xs) -> openCells xs $ openCell' x b
    openCell' coord b =
      if isHiddenCell coord b
        then let nb = openCell coord b in openAdjBlank0Cells coord nb
        else b

mineSweeperApp :: IO (App MSState)
mineSweeperApp = do
  pure $
    App
      { appName = "MineSweeper",
        appGenState,
        appRender = renderApp,
        appHandleEvent = handleEvent
      }
  where
    appGenState = do
      board <- initBoard
      pure $ MSState board Wait

diffTimeToFloat :: UTCTime -> UTCTime -> Float
diffTimeToFloat a b = realToFrac $ diffUTCTime a b

wSEvent :: WSEvent -> IO (Maybe MSEvent)
wSEvent (WSEvent wseName _ wseData) = case wseName of
  "showCell" -> do
    case (getData "cx", getData "cy") of
      (Just (Just cxS), Just (Just cyS)) -> do
        let cxM = readMaybe $ from cxS :: Maybe Int
            cyM = readMaybe $ from cyS :: Maybe Int
        case (cxM, cyM) of
          (Just cx, Just cy) -> do
            now <- getCurrentTime
            pure . Just $ OpenCell (MSCellCoord cx cy) now
          _ -> pure Nothing
      _ -> pure Nothing
  "play" -> do
    newBoard <- initBoard
    pure $ Just $ NewGame newBoard
  _ -> pure Nothing
  where
    getData = flip Map.lookup wseData

handleEvent :: WSEvent -> TVar MSState -> IO [Html ()]
handleEvent wEv appStateV = do
  evM <- wSEvent wEv
  case evM of
    Just (OpenCell cellCoord atTime) -> do
      appState' <- readTVarIO appStateV
      case appState'.state of
        Wait -> atomically $ modifyTVar' appStateV $ \s -> s {state = Play atTime}
        _ -> pure ()
      appState <- readTVarIO appStateV
      let playDuration = mkPlayDuration appState.state atTime
      case isMineCell cellCoord appState.board of
        True -> do
          (board, panel) <- atomically $ do
            writeTVar appStateV $ MSState (openCell cellCoord appState.board) Gameover
            board <- renderBoard appStateV
            panel <- renderPanel appStateV (Just playDuration)
            pure (board, panel)
          pure [board, panel]
        False -> do
          let gs1 = openCell cellCoord appState.board
              gs2 = openAdjBlank0Cells cellCoord gs1
          case countHiddenBlank gs2 == 0 of
            True -> do
              (board, panel) <- atomically $ do
                modifyTVar' appStateV $ \s -> s {board = gs2, state = Win}
                board <- renderBoard appStateV
                panel <- renderPanel appStateV (Just playDuration)
                pure (board, panel)
              pure [board, panel]
            False -> do
              (board, smiley) <- atomically $ do
                modifyTVar' appStateV $ \s -> s {board = gs2}
                board <- renderBoard appStateV
                smiley <- renderSmiley appStateV
                pure (board, smiley)
              pure [board, smiley]
    Just (NewGame newBoard) -> do
      atomically $ writeTVar appStateV $ MSState newBoard Wait
      app <- atomically $ renderApp appStateV
      pure [app]
    _ -> pure []
  where
    -- UpdateTimer diffT -> do
    --   pure [renderTimer diffT]

    mkPlayDuration :: MSGameState -> UTCTime -> Float
    mkPlayDuration s curD = case s of
      Play startDate -> diffTimeToFloat curD startDate
      _ -> error "Should not happen"

renderApp :: TVar MSState -> STM (Html ())
renderApp appStateV = do
  panel <- renderPanel appStateV (Just 0.0)
  board <- renderBoard appStateV
  pure $ div_ [id_ "MSMain", class_ "w-60 border-2 border-gray-400 bg-gray-100"] $ do
    panel
    board

renderPanel :: TVar MSState -> Maybe Float -> STM (Html ())
renderPanel appStateV durationM = do
  smiley <- renderSmiley appStateV
  pure $ div_ [id_ "MSPanel", class_ "bg-gray-200 m-1 flex justify-between"] $ do
    div_ [class_ "w-10"] "9 💣"
    smiley
    case durationM of
      Just duration -> renderTimer duration
      _ -> pure ()

renderSmiley :: TVar MSState -> STM (Html ())
renderSmiley appStateV = do
  appState <- readTVar appStateV
  pure $
    div_ [id_ "MSSmiley"] $
      withEvent "play" [] $
        div_ [class_ "bg-gray-300 border-2 cursor-pointer"] $ case appState.state of
          Play _ -> "🙂"
          Wait -> "😴"
          Gameover -> "😖"
          Win -> "😎"

renderTimer :: Float -> Html ()
renderTimer duration = do
  let durationT = printf "%.1f" duration :: String
  div_ [id_ "MSTimer", class_ "w-10 text-right"] $ toHtml durationT

renderBoard :: TVar MSState -> STM (Html ())
renderBoard appStateV = do
  appState <- readTVar appStateV
  pure $ div_ [id_ "MSBoard", class_ "grid grid-cols-10 gap-1"] $ do
    mapM_ (renderCell appState.state) $ Map.toList appState.board
  where
    renderCell :: MSGameState -> (MSCellCoord, MSCell) -> Html ()
    renderCell gameState (cellCoords, cellState) =
      let cellId = mkHxVals [("cx", pack $ show $ cellCoords.cx), ("cy", pack $ show $ cellCoords.cy)]
       in installCellEvent gameState cellId $
            div_ [class_ "bg-gray-300 text-center cursor-pointer"] $
              case cellState of
                MSCell (Blank v) Open
                  | v == 0 -> div_ [class_ "bg-gray-200 h-6 w-full "] $ ""
                  | v == 1 -> div_ [class_ "bg-gray-200 font-bold text-blue-700"] $ showCellValue v
                  | v == 2 -> div_ [class_ "bg-gray-200 font-bold text-green-700"] $ showCellValue v
                  | v == 3 -> div_ [class_ "bg-gray-200 font-bold text-red-700"] $ showCellValue v
                  | v == 4 -> div_ [class_ "bg-gray-200 font-bold text-blue-900"] $ showCellValue v
                  | v == 5 -> div_ [class_ "bg-gray-200 font-bold text-red-900"] $ showCellValue v
                  | v == 6 -> div_ [class_ "bg-gray-200 font-bold text-green-900"] $ showCellValue v
                  | v == 7 -> div_ [class_ "bg-gray-200 font-bold text-brown-700"] $ showCellValue v
                  | v == 8 -> div_ [class_ "bg-gray-200 font-bold text-black-700"] $ showCellValue v
                MSCell (Blank _) Open -> error "Impossible case"
                MSCell Mine Open -> div_ [class_ "bg-red-500"] "💣"
                MSCell _ Hidden -> div_ [class_ "border-2 border-r-gray-400 border-b-gray-400 h-6 w-full"] ""
      where
        showCellValue :: Int -> Html ()
        showCellValue = toHtml . show
        installCellEvent :: MSGameState -> Attribute -> Html () -> Html ()
        installCellEvent gs cellId elm =
          let elm' = withEvent "showCell" [cellId] elm
           in case gs of
                Play _ -> elm'
                Wait -> elm'
                _ -> elm

run :: IO ()
run = do
  app <- mineSweeperApp
  runServer app
