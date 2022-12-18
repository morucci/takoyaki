{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Demo.HazardHunter where

import Codec.Serialise (Serialise)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import qualified Data.Map as Map
import Data.Text (Text, intercalate, pack)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.Internal as DB
import qualified Database.SQLite.Simple.Ok as DB
import GHC.Generics (Generic)
import qualified Ki
import Lucid
import qualified Network.WebSockets as WS
import System.Random (randomRIO)
import Takoyaki.Engine
import Takoyaki.Htmx
import Text.Printf (printf)
import Text.Read (readMaybe)
import Witch
import Prelude

data MSState = MSState
  { board :: MSBoard,
    state :: MSGameState,
    settings :: MSSettings
  }
  deriving (Show, Generic)

instance Serialise MSState

data MSSettings = MSSettings
  { level :: MSLevel,
    playerName :: Text,
    hazard :: Hazard
  }
  deriving (Show, Generic)

instance Serialise MSSettings

data MSBoardSettings = MSBoardSettings
  { sizeCount :: Int,
    mineCount :: Int
  }

data MSGameState
  = Play UTCTime Bool
  | Win
  | Gameover
  | Wait
  deriving (Show, Generic)

instance Serialise MSGameState

data MSCellContent
  = Mine
  | Blank Int
  deriving (Show, Generic)

instance Serialise MSCellContent

data MSCellStatus
  = Open
  | Hidden Bool
  deriving (Show, Generic)

instance Serialise MSCellStatus

data MSCell = MSCell
  { cellContent :: MSCellContent,
    cellStatus :: MSCellStatus
  }
  deriving (Show, Generic)

instance Serialise MSCell

data MSCellCoord = MSCellCoord
  { cx :: Int,
    cy :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Serialise MSCellCoord

type MSBoard = Map.Map MSCellCoord MSCell

data MSEvent
  = NewGame
  | ClickCell MSCellCoord
  | SettingsSelected MSLevel Text
  | SetFlagMode

data MSLevel
  = Baby
  | Beginner
  | Intermediate
  | Expert
  deriving (Bounded, Eq, Enum, Show, Generic)

instance Serialise MSLevel

instance From Text MSLevel where
  from txt = case txt of
    "Baby" -> Baby
    "Beginner" -> Beginner
    "Intermediate" -> Intermediate
    "Expert" -> Expert
    _ -> error "Unhandled level"

data Hazard
  = HMine
  | HSnake
  | HSpidder
  | HPumpkin
  | HPoo
  | HVampire
  | HTengu
  | HAlien
  | HAlien2
  | HGost
  deriving (Bounded, Enum, Show, Generic)

instance Serialise Hazard

hazards :: [Hazard]
hazards = [minBound .. maxBound]

randomHazard :: IO Hazard
randomHazard = do
  selected <- randomRIO (0, length hazards - 1)
  pure $ hazards !! selected

hazardToText :: Hazard -> Text
hazardToText hazard = case hazard of
  HMine -> "💣"
  HSnake -> "🐍" 
  HSpidder -> "🕷"
  HPumpkin -> "🎃"
  HPoo -> "💩"
  HVampire -> "🧛"
  HTengu -> "👺"
  HAlien -> "👽"
  HAlien2 -> "👾"
  HGost -> "👻"

defaultLevel :: MSLevel
defaultLevel = Beginner

levelToBoardSettings :: MSLevel -> MSBoardSettings
levelToBoardSettings level = case level of
  Baby -> MSBoardSettings 6 3
  Beginner -> MSBoardSettings 9 9
  Intermediate -> MSBoardSettings 15 30
  Expert -> MSBoardSettings 15 50

initBoard :: MSBoardSettings -> IO MSBoard
initBoard settings@MSBoardSettings {..} = do
  let cellsCoords = [MSCellCoord x y | x <- [0 .. sizeCount], y <- [0 .. sizeCount]]
      blankBoard = Map.fromList $ map (\coord -> (coord, MSCell (Blank 0) (Hidden False))) cellsCoords
  minesCoords <- getMinesCoords cellsCoords []
  pure $ setBoard blankBoard minesCoords
  where
    getMinesCoords :: [MSCellCoord] -> [MSCellCoord] -> IO [MSCellCoord]
    getMinesCoords availableCellsCords minesCoords = do
      if length minesCoords == mineCount
        then pure minesCoords
        else do
          selectedIndex <- randomRIO (0, length availableCellsCords - 1)
          let selectedCoord = availableCellsCords !! selectedIndex
              remainingCellsCords = filter (\cell -> not $ cell == selectedCoord) availableCellsCords
          getMinesCoords remainingCellsCords (minesCoords <> [selectedCoord])
    setBoard :: MSBoard -> [MSCellCoord] -> MSBoard
    setBoard board minesCoords =
      let adjCellds = concatMap (getAdjCellCoords settings) minesCoords
          board' = installAdjCells board adjCellds
       in installMines board' minesCoords
      where
        installMines :: MSBoard -> [MSCellCoord] -> MSBoard
        installMines b cs = case cs of
          [] -> b
          [x] -> Map.insert x (MSCell Mine (Hidden False)) b
          (x : xs) -> installMines (Map.insert x (MSCell Mine (Hidden False)) b) (xs)
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
            (MSCell (Blank 1) (Hidden False))
            b

getAdjCellCoords :: MSBoardSettings -> MSCellCoord -> [MSCellCoord]
getAdjCellCoords MSBoardSettings {..} MSCellCoord {..} =
  let isInBoard (MSCellCoord cx' cy') =
        cx' >= 0
          && cx' <= sizeCount
          && cy' >= 0
          && cy' <= sizeCount
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

setFlagOnCell :: MSCellCoord -> MSBoard -> MSBoard
setFlagOnCell cellCoord board = Map.update func cellCoord board
  where
    func :: MSCell -> Maybe MSCell
    func (MSCell content (Hidden flagState)) = Just $ MSCell content (Hidden (not flagState))
    func cell = Just cell

getCell :: MSCellCoord -> MSBoard -> Maybe MSCell
getCell = Map.lookup

isBlank0Cell :: MSCellCoord -> MSBoard -> Bool
isBlank0Cell cellCoord board = case getCell cellCoord board of
  Just (MSCell (Blank 0) _) -> True
  _ -> False

isHiddenCell :: MSCellCoord -> MSBoard -> Bool
isHiddenCell cellCoord board = case getCell cellCoord board of
  Just (MSCell _ (Hidden False)) -> True
  _ -> False

isMineCell :: MSCellCoord -> MSBoard -> Bool
isMineCell cellCoord board = case getCell cellCoord board of
  Just (MSCell Mine _) -> True
  _ -> False

isFlagCell :: MSCellCoord -> MSBoard -> Bool
isFlagCell cellCoord board = case getCell cellCoord board of
  Just (MSCell _ (Hidden True)) -> True
  _ -> False

countHiddenBlank :: MSBoard -> Int
countHiddenBlank board = length (filter keepHiddenBlank (Map.elems board))
  where
    keepHiddenBlank (MSCell (Blank _) (Hidden False)) = True
    keepHiddenBlank _ = False

countFlagCells :: MSBoard -> Int
countFlagCells board = length (filter keepFlagCell (Map.elems board))
  where
    keepFlagCell (MSCell _ (Hidden True)) = True
    keepFlagCell _ = False

countOpenCells :: MSBoard -> Int
countOpenCells board = length (filter keepOpenCell (Map.elems board))
  where
    keepOpenCell (MSCell _ Open) = True
    keepOpenCell _ = False

openAdjBlank0Cells :: MSBoardSettings -> MSCellCoord -> MSBoard -> MSBoard
openAdjBlank0Cells settings cellCoord board =
  if isBlank0Cell cellCoord board
    then openCells (getAdjCellCoords settings cellCoord) board
    else board
  where
    openCells :: [MSCellCoord] -> MSBoard -> MSBoard
    openCells cellsCoords b = case cellsCoords of
      [] -> b
      [x] -> openCell' x b
      (x : xs) -> openCells xs $ openCell' x b
    openCell' coord b =
      if isHiddenCell coord b
        then let nb = openCell coord b in openAdjBlank0Cells settings coord nb
        else b

data ServiceEvent
  = StartTimer
  | StopTimer
  deriving (Show)

hazardHunterApp :: IO (App MSState ServiceEvent)
hazardHunterApp = do
  pure $
    App
      { appName = "HazardHunter",
        appMkSessionState,
        appInitDB,
        appRender = renderApp,
        appHandleEvent = handleEvent,
        appService = serviceThread
      }
  where
    appMkSessionState = do
      let level = defaultLevel
      board <- initBoard $ levelToBoardSettings level
      hazard <- randomHazard
      pure $ MSState board Wait $ MSSettings level "Anonymous" hazard
    appInitDB conn = do
      DB.execute_
        conn
        "CREATE TABLE IF NOT EXISTS scores (id INTEGER PRIMARY KEY, name TEXT, date DATE, duration REAL, level TEXT)"

data Score = Score
  { scoreId :: Int,
    scoreName :: Text,
    scoreDate :: UTCTime,
    scoreDuration :: Float,
    scoreLevel :: MSLevel
  }
  deriving (Show)

instance DB.FromRow Score where
  fromRow = Score <$> DB.field <*> DB.field <*> DB.field <*> DB.field <*> DB.field

instance DB.FromField MSLevel where
  fromField (DB.Field (DB.SQLText txt) _) = DB.Ok . from $ txt
  fromField f = DB.returnError DB.ConversionFailed f "need a valid text level"

getTopScores :: DB.Connection -> Integer -> MSLevel -> IO [Score]
getTopScores conn limit level =
  DB.query
    conn
    "SELECT * from scores WHERE level = ? ORDER BY duration ASC LIMIT ?"
    (show level, show limit)

addScore :: DB.Connection -> Text -> UTCTime -> Float -> MSLevel -> IO ()
addScore conn name date duration level =
  DB.execute
    conn
    "INSERT INTO scores (name, date, duration, level) VALUES (?,?,?,?)"
    (name, date, duration, show level)

diffTimeToFloat :: UTCTime -> UTCTime -> Float
diffTimeToFloat a b = realToFrac $ diffUTCTime a b

serviceThread :: TVar MSState -> TBQueue ServiceEvent -> WS.Connection -> IO ()
serviceThread stateV serviceQ conn = do
  timerState <- newTVarIO Nothing
  appState' <- readTVarIO stateV
  if (isPlaying appState'.state)
    then atomically $ writeTBQueue serviceQ StartTimer
    else pure ()
  Ki.scoped $ \scope -> do
    Ki.fork_ scope $ forever $ do
      event <- atomically $ readTBQueue serviceQ
      case event of
        StartTimer -> do
          atomically $ do
            appState <- readTVar stateV
            writeTVar timerState $ getPlayDate appState.state
          void $ Ki.fork scope $ timerT timerState
        StopTimer -> atomically $ writeTVar timerState Nothing
    atomically $ Ki.awaitAll scope
  where
    timerT :: TVar (Maybe UTCTime) -> IO ()
    timerT timerState = go
      where
        go = do
          ts <- readTVarIO timerState
          case ts of
            Just atTime -> do
              now <- getCurrentTime
              WS.sendTextData conn
                . renderBS
                . renderTimer
                $ diffTimeToFloat now atTime
              threadDelay 500000
              go
            Nothing -> pure ()
    getPlayDate :: MSGameState -> Maybe UTCTime
    getPlayDate (Play date _) = Just date
    getPlayDate _ = Nothing
    isPlaying :: MSGameState -> Bool
    isPlaying (Play _ _) = True
    isPlaying _ = False

handleEvent :: WSEvent -> TVar MSState -> TBQueue ServiceEvent -> DB.Connection -> IO [Html ()]
handleEvent wEv appStateV serviceQ dbConn = do
  case wSEvent wEv of
    Just (ClickCell cellCoord) -> do
      atTime <- getCurrentTime
      appState' <- readTVarIO appStateV
      case countOpenCells appState'.board of
        0 -> do
          -- Ensure the first click on the board is not a hazard
          newBoard <- ensureNFBoard appState'.board cellCoord appState'.settings.level
          atomically $ modifyTVar' appStateV $ \s -> s {board = newBoard}
          pure ()
        _ -> pure ()
      case appState'.state of
        Wait -> atomically $ do
          modifyTVar' appStateV $ \s -> s {state = Play atTime False}
          writeTBQueue serviceQ StartTimer
        _ -> pure ()
      appState <- readTVarIO appStateV
      case appState.state of
        Play _ False -> do
          let playDuration = mkPlayDuration appState.state atTime
          case isFlagCell cellCoord appState.board of
            True -> pure []
            False -> do
              case isMineCell cellCoord appState.board of
                True -> do
                  (board, panel) <- atomically $ do
                    writeTBQueue serviceQ StopTimer
                    modifyTVar' appStateV $ \s ->
                      s
                        { board = openCell cellCoord appState.board,
                          state = Gameover
                        }
                    board <- renderBoard appStateV
                    panel <- renderPanel appStateV (Just playDuration)
                    pure (board, panel)
                  pure [board, panel]
                False -> do
                  let gs1 = openCell cellCoord appState.board
                      gs2 = openAdjBlank0Cells (levelToBoardSettings appState.settings.level) cellCoord gs1
                  case countHiddenBlank gs2 == 0 of
                    True -> do
                      (board, panel) <- atomically $ do
                        writeTBQueue serviceQ StopTimer
                        modifyTVar' appStateV $ \s -> s {board = gs2, state = Win}
                        board <- renderBoard appStateV
                        panel <- renderPanel appStateV (Just playDuration)
                        pure (board, panel)
                      addScore dbConn appState.settings.playerName atTime playDuration appState.settings.level
                      leaderBoard <- renderLeaderBoard appStateV dbConn
                      pure [board, panel, leaderBoard]
                    False -> do
                      (board, smiley) <- atomically $ do
                        modifyTVar' appStateV $ \s -> s {board = gs2}
                        board <- renderBoard appStateV
                        smiley <- renderSmiley appStateV
                        pure (board, smiley)
                      pure [board, smiley]
        Play _ True -> atomically $ do
          let board = setFlagOnCell cellCoord appState.board
          modifyTVar' appStateV $ \s -> s {board}
          flag <- renderFlag appStateV
          board' <- renderBoard appStateV
          pure [flag, board']
        _ -> pure []
    Just NewGame -> atomically $ do
      writeTBQueue serviceQ StopTimer
      panel <- renderPanel appStateV (Just 0.0)
      levelsSelector <- renderSettings appStateV
      pure [panel, levelsSelector]
    Just (SettingsSelected level playerName) -> do
      newBoard <- initBoard $ levelToBoardSettings level
      hazard <- randomHazard
      atomically $ do
        modifyTVar' appStateV $ \s ->
          s
            { board = newBoard,
              state = Wait,
              settings = MSSettings level playerName hazard
            }
      app <- renderApp appStateV dbConn
      pure [app]
    Just SetFlagMode -> do
      frags <- atomically $ do
        appState <- readTVar appStateV
        case appState.state of
          Play st fm -> do
            modifyTVar' appStateV $ \s -> s {state = Play st (not fm)}
            renderFlag appStateV
          _ -> pure $ pure ()
      pure [frags]
    Nothing -> pure []
  where
    mkPlayDuration :: MSGameState -> UTCTime -> Float
    mkPlayDuration s curD = case s of
      Play startDate _ -> diffTimeToFloat curD startDate
      _ -> error "Should not happen"
    wSEvent :: WSEvent -> Maybe MSEvent
    wSEvent (WSEvent wseName _ wseData) = do
      let getData = flip Map.lookup wseData
      case wseName of
        "clickCell" -> do
          case (getData "cx", getData "cy") of
            (Just (Just cxS), Just (Just cyS)) -> do
              let cxM = readMaybe $ from cxS :: Maybe Int
                  cyM = readMaybe $ from cyS :: Maybe Int
              case (cxM, cyM) of
                (Just cx, Just cy) -> Just $ ClickCell (MSCellCoord cx cy)
                _ -> Nothing
            _ -> Nothing
        "play" -> Just NewGame
        "setSettings" -> do
          case (getData "level", getData "playerName") of
            ((Just (Just level)), (Just (Just playerName))) -> Just $ SettingsSelected (from level) playerName
            _ -> Nothing
        "setFlagMode" -> Just SetFlagMode
        _ -> Nothing
    ensureNFBoard :: MSBoard -> MSCellCoord -> MSLevel -> IO MSBoard
    ensureNFBoard board cellCoord level = case isMineCell cellCoord board of
      True -> do
        newBoard <- initBoard $ levelToBoardSettings level
        ensureNFBoard newBoard cellCoord level
      False -> pure board

withThemeBgColor :: Text -> Text -> Text
withThemeBgColor level cur = cur <> " " <> bgColorBase <> "-" <> level
  where
    colorBase = "blue"
    bgColorBase = "bg-" <> colorBase

renderApp :: TVar MSState -> DB.Connection -> IO (Html ())
renderApp appStateV dbConn = do
  (panel, board) <- atomically $ do
    panel <- renderPanel appStateV (Just 0.0)
    board <- renderBoard appStateV
    pure (panel, board)
  leaderBoard <- renderLeaderBoard appStateV dbConn
  appState <- readTVarIO appStateV
  pure $ div_ [id_ "MSMain", class_ "min-w-fit max-w-fit border-2 rounded border-gray-400 bg-gray-100"] $ do
    div_ [class_ "flex flex-col"] $ do
      div_ [class_ "border-solid rounded border-2 m-1 border-gray-300"] $ do
        panel
        board
      div_ [class_ "border-solid rounded border-2 m-1 border-gray-300"] $ do
        renderLeaderBoardHeader appState.settings.level
        leaderBoard
      div_ [class_ $ withThemeBgColor "200" ""] $ do
        div_ [class_ "flex flex-row gap-2 flex-row-reverse pr-2"] $ do
          div_ [] "- 1.0.0"
          a_ [class_ "text-blue-600", href_ "https://github.com/morucci/takoyaki"] "HazardHunter"

renderLeaderBoardHeader :: MSLevel -> Html ()
renderLeaderBoardHeader level =
  div_ [class_ $ withThemeBgColor "200" "text-center"] $ (toHtml $ show level) <> " " <> "Leaderboard"

renderLeaderBoard :: TVar MSState -> DB.Connection -> IO (Html ())
renderLeaderBoard appStateV dbConn = do
  appState <- readTVarIO appStateV
  scores <- getTopScores dbConn 10 appState.settings.level
  pure $ div_ [id_ "MSLeaderBoard", class_ $ withThemeBgColor "100" ""] $ case length scores of
    0 -> p_ "The leaderboard is empty. Be the first to appear here !"
    _ -> ol_ [] $ mapM_ displayScoreLine scores
  where
    displayScoreLine :: Score -> Html ()
    displayScoreLine Score {..} = do
      li_ [] $ div_ [class_ "grid grid-cols-5 gap-1"] $ do
        div_ [class_ "col-span-4"] $
          toHtml $
            formatTime defaultTimeLocale "%F" scoreDate <> " " <> from scoreName
        div_ [class_ "col-span-1 text-right"] $ toHtml (toDurationT scoreDuration)

renderPanel :: TVar MSState -> Maybe Float -> STM (Html ())
renderPanel appStateV durationM = do
  smiley <- renderSmiley appStateV
  flag <- renderFlag appStateV
  appState <- readTVar appStateV
  pure $ div_ [id_ "MSPanel", class_ $ withThemeBgColor "200" "flex justify-between"] $ do
    let mineCount' = mineCount $ levelToBoardSettings appState.settings.level
    div_ [class_ "pl-1 w-24"] $ toHtml $ hazardLabel mineCount' appState.settings.hazard
    div_ [class_ "flex flex-row gap-2"] $ do
      smiley
      div_ [class_ "p-2"] ""
      flag
    case durationM of
      Just duration -> renderTimer duration
      _ -> pure ()
  where
    hazardLabel :: Int -> Hazard -> Text
    hazardLabel count hazard = (from $ show count) <> " " <> hazardToText hazard

renderFlag :: TVar MSState -> STM (Html ())
renderFlag appStateV = do
  appState <- readTVar appStateV
  let flagMode = case appState.state of
        Play _ True -> "bg-red-200"
        _ -> mempty
      usedFlags = countFlagCells appState.board
  pure $ div_ [id_ "MSFlag"] $ do
    div_ [class_ "flex flex-row gap-1"] $ do
      withEvent "setFlagMode" [] $ div_ [class_ ("cursor-pointer " <> flagMode)] "🚩"
      div_ . toHtml $ "(" <> show usedFlags <> ")"

renderSmiley :: TVar MSState -> STM (Html ())
renderSmiley appStateV = do
  appState <- readTVar appStateV
  pure $
    div_ [id_ "MSSmiley"] $
      withEvent "play" [] $
        div_ [class_ "cursor-pointer"] $ case appState.state of
          Play _ _ -> "🤔"
          Wait -> "💤"
          Gameover -> "😖"
          Win -> "🥳"

toDurationT :: Float -> String
toDurationT duration = printf "%.1f" duration

renderTimer :: Float -> Html ()
renderTimer duration = do
  div_ [id_ "MSTimer", class_ "w-24 text-right pr-1"] $ toHtml $ toDurationT duration

renderSettings :: TVar MSState -> STM (Html ())
renderSettings appStateV = do
  appState <- readTVar appStateV
  let playerName = appState.settings.playerName
      selectedLevel = appState.settings.level
  pure $ div_ [id_ "MSBoard"] $ do
    withEvent "setSettings" [] $ do
      form_ [class_ $ withThemeBgColor "100" "flex flex-col items-center gap-px"] $ do
        label_ [class_ "m-1 font-semibold"] "Set your name"
        nameInput playerName
        label_ [class_ "m-1 font-semibold"] "Select a level"
        mapM_ (div_ . levelButton selectedLevel) [minBound .. maxBound]
        div_ [class_ "my-2"] $ submitButton
  where
    nameInput :: Text -> Html ()
    nameInput playerName =
      input_
        [ type_ "text",
          name_ "playerName",
          value_ playerName,
          placeholder_ "Anonymous",
          size_ "15",
          maxlength_ "15",
          class_ "h-8 text-center border border-slate-300 rounded-md focus:border-slate-400"
        ]
    submitButton :: Html ()
    submitButton =
      button_
        [ type_ "submit",
          class_ "p-1 border-2 border-green-400 rounded"
        ]
        "Start a new board"
    levelButton :: MSLevel -> MSLevel -> Html ()
    levelButton selectedLevel level = do
      input_ $
        [name_ "level", id_ levelT, type_ "radio", value_ levelT]
          <> if level == selectedLevel then [checked_] else mempty
      label_ [for_ levelT] $ span_ [class_ levelS] $ toHtml levelT
      where
        levelT :: Text
        levelT = from $ show level
        levelS =
          "ml-2" <> " " <> case level of
            Baby -> "text-blue-700"
            Beginner -> "text-blue-800"
            Intermediate -> "text-green-700"
            Expert -> "text-red-700"

renderBoard :: TVar MSState -> STM (Html ())
renderBoard appStateV = do
  appState <- readTVar appStateV
  let sizeCount' = sizeCount $ levelToBoardSettings appState.settings.level
  let gridType = "grid-cols-[" <> (intercalate "_" $ Prelude.replicate (sizeCount' + 1) "20px") <> "]"
  pure $ div_ [id_ "MSBoard"] $ do
    div_ [class_ "flex place-content-center m-1"] $ do
      div_ [class_ $ "grid gap-1 " <> gridType] $ do
        mapM_ (renderCell appState.state appState.settings.hazard) $
          Map.toList appState.board
  where
    renderCell :: MSGameState -> Hazard -> (MSCellCoord, MSCell) -> Html ()
    renderCell gameState hazard (cellCoords, cellState) =
      let cellId = mkHxVals [("cx", pack $ show $ cellCoords.cx), ("cy", pack $ show $ cellCoords.cy)]
       in installCellEvent gameState cellId $
            div_ [class_ $ withThemeBgColor "100" "text-center cursor-pointer"] $
              case cellState of
                MSCell (Blank v) Open
                  | v == 0 -> div_ [class_ "h-6 w-full "] $ ""
                  | v == 1 -> div_ [class_ "font-bold text-blue-700"] $ showCellValue v
                  | v == 2 -> div_ [class_ "font-bold text-green-700"] $ showCellValue v
                  | v == 3 -> div_ [class_ "font-bold text-red-700"] $ showCellValue v
                  | v == 4 -> div_ [class_ "font-bold text-blue-900"] $ showCellValue v
                  | v == 5 -> div_ [class_ "font-bold text-red-900"] $ showCellValue v
                  | v == 6 -> div_ [class_ "font-bold text-green-900"] $ showCellValue v
                  | v == 7 -> div_ [class_ "font-bold text-brown-700"] $ showCellValue v
                  | v == 8 -> div_ [class_ "font-bold text-black-700"] $ showCellValue v
                MSCell (Blank _) Open -> error "Impossible case"
                MSCell Mine Open -> mineCell
                MSCell (Blank _) (Hidden True) -> flagCell
                MSCell Mine (Hidden flag) -> case gameState of
                  Gameover -> mineCell
                  _ -> if flag then flagCell else hiddenCell
                MSCell _ _ -> hiddenCell
      where
        mineCell = div_ [class_ "bg-red-500"] $ toHtml $ hazardToText hazard
        hiddenCell = div_ [class_ $ withThemeBgColor "300" "border-2 rounded border-r-gray-400 border-b-gray-400 h-6 w-full"] ""
        flagCell = div_ [class_ $ withThemeBgColor "300" "border-2 rounded border-r-gray-400 border-b-gray-400 h-6 w-full"] "🚩"
        showCellValue :: Int -> Html ()
        showCellValue = toHtml . show
        installCellEvent :: MSGameState -> Attribute -> Html () -> Html ()
        installCellEvent gs cellId elm =
          let elm' = withEvent "clickCell" [cellId] elm
           in case gs of
                Play _ _ -> elm'
                Wait -> elm'
                _ -> elm

run :: Int -> IO ()
run port = do
  app <- hazardHunterApp
  runServer port app
