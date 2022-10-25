{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Demo.Todo where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, parseTimeOrError)
import Lucid
import System.Random
import Takoyaki.Engine
import Takoyaki.Htmx (mkHxVals)
import Witch
import Prelude

type TaskId = Text

data TaskPrio = High | Medium | Low deriving (Show, Eq)

data Task = Task
  { taskId :: TaskId,
    taskData :: Text,
    taskDate :: UTCTime,
    taskPrio :: TaskPrio
  }
  deriving (Show, Eq)

data TaskUpdate = TaskUpdate
  { taskUpdateData :: Text,
    taskUpdatePrio :: TaskPrio
  }
  deriving (Show, Eq)

data TodoList = TodoList [Task] deriving (Show)

data TodoAPPEvent
  = AddTask Task
  | DelTask TaskId
  | EditTask TaskId
  | UpdateTask TaskId TaskUpdate
  | RenderAddTask
  deriving (Show)

instance From Text TaskPrio where
  from txt = case txt of
    "High" -> High
    "Medium" -> Medium
    "Low" -> Low
    _otherwise -> Low

instance From TaskPrio Text where
  from txt = case txt of
    High -> "High"
    Medium -> "Medium"
    Low -> "Low"

data Service

todoApp :: TVar TodoList -> App TodoList TodoAPPEvent Service
todoApp appState =
  App
    { appName = "Takoyaki Todo",
      appWSEvent = todoWSEvent,
      appState,
      appRender = renderApp,
      appHandleEvent = todoHandleEvent,
      appService = const . const $ pure ()
    }

todoWSEvent :: WSEvent -> Maybe ([IO TodoAPPEvent])
todoWSEvent (WSEvent wseName _ wseData) = case wseName of
  "AddTask" -> do
    case (getData "task-name", getData "task-prio") of
      (Just (Just taskData), Just (Just taskPrio)) -> do
        Just [buildAddTaskEvent taskData taskPrio]
      _ -> Nothing
  "DelTask" -> do
    case getData "TaskId" of
      (Just (Just taskId)) -> Just [pure $ DelTask taskId]
      _ -> Nothing
  "EditTask" -> do
    case getData "TaskId" of
      (Just (Just taskId)) -> Just [pure $ EditTask taskId]
      _ -> Nothing
  "UpdateTask" -> do
    case (getData "task-name", getData "task-prio", getData "taskId") of
      (Just (Just taskData), Just (Just taskPrio), Just (Just taskId)) -> do
        let taskUpdate = TaskUpdate taskData (from taskPrio)
        Just [pure $ UpdateTask taskId taskUpdate, pure $ RenderAddTask]
      _ -> Nothing
  _ -> Nothing
  where
    getData = flip Map.lookup wseData
    buildAddTaskEvent :: Text -> Text -> IO TodoAPPEvent
    buildAddTaskEvent taskData taskPrio = do
      now <- getCurrentTime
      taskIdI <- getRandom
      pure . AddTask $ Task (from $ show taskIdI) taskData now (from taskPrio)
      where
        getRandom :: IO Int
        getRandom = randomIO

todoHandleEvent :: TodoAPPEvent -> TVar TodoList -> ServiceQ Service -> IO [Html ()]
todoHandleEvent ev appStateV _serviceQ = do
  appState <- readTVarIO appStateV
  case ev of
    AddTask task -> do
      todoListR <- atomically $ do
        writeTVar appStateV $ addTask appState task
        todoListH appStateV
      pure [todoListR]
    DelTask taskId -> do
      todoListR <- atomically $ do
        writeTVar appStateV $ delTask appState taskId
        todoListH appStateV
      pure [todoListR]
    EditTask taskId -> do
      case getTask appState taskId of
        Just task -> pure [editTaskFormH task]
        Nothing -> pure []
    UpdateTask taskId taskUpdate -> do
      todoListR <- atomically $ do
        writeTVar appStateV $ updateTask appState taskId taskUpdate
        todoListH appStateV
      pure [todoListR]
    RenderAddTask -> pure [addTaskFormH]

renderApp :: TVar TodoList -> STM (Html ())
renderApp appStateV = do
  todoListR <- todoListH appStateV
  pure $ div_ [class_ "bg-gray-200"] $ do
    h1_ "Takoyaki TODO list demo"
    addTaskFormH
    todoListR

addTaskFormH :: Html ()
addTaskFormH =
  div_ [id_ "todo-control", class_ "bg-gray-100"] $ do
    withEvent "AddTask" [] $ do
      form_ $ do
        span_ [class_ "mr-2"] $ do
          label_ [for_ "task-name", class_ "mr-1"] "Task"
          input_ [name_ "task-name", type_ "text"]
        span_ [class_ "mr-2"] $ prioSelectH Nothing
        button_ [type_ "submit"] "Add task"

editTaskFormH :: Task -> Html ()
editTaskFormH task =
  div_ [id_ "todo-control", class_ "bg-gray-100"] $ do
    withEvent "UpdateTask" [mkHxVals [("taskId", task.taskId)]] $ do
      form_ $ do
        span_ [class_ "mr-2"] $ do
          label_ [for_ "task-name", class_ "mr-1"] "Task"
          input_ [name_ "task-name", type_ "text", value_ task.taskData]
        span_ [class_ "mr-2"] $ prioSelectH . Just $ from task.taskPrio
        button_ [type_ "submit"] "Update task"

prioSelectH :: Maybe Text -> Html ()
prioSelectH selectedItem = do
  label_ [for_ "todo-input-prio", class_ "mr-1"] "Task prio"
  select_ [name_ "task-prio", id_ "todo-input-prio"] $ do
    option_ (oValue "High") "High"
    option_ (oValue "Medium") "Medium"
    option_ (oValue "Low") "Low"
  where
    oValue :: Text -> [Attribute]
    oValue item
      | Just item == selectedItem = [value_ item, selected_ ""]
      | otherwise = [value_ item]

todoListH :: TVar TodoList -> STM (Html ())
todoListH appStateV = do
  appState <- readTVar appStateV
  pure $ div_ [id_ "todoList"] $ mapM_ renderTask (getTasks appState)
  where
    renderTask :: Task -> Html ()
    renderTask task = do
      let taskId = mkHxVals [("TaskId", task.taskId)]
      div_ $ span_ $ do
        withEvent "DelTask" [taskId] $ do
          button_ [class_ "mr-2 bg-red-600"] "Del"
        withEvent "EditTask" [taskId] $ do
          button_ [class_ "mr-2 bg-yellow-600"] "Edit"
        span_ [class_ "mr-2"] $ toHtml $ formatTime defaultTimeLocale "%F" task.taskDate
        span_ [class_ "mr-2"] $ toHtml task.taskData
        span_ [class_ "mr-2"] $ toHtml $ show task.taskPrio

addTask :: TodoList -> Task -> TodoList
addTask (TodoList tasks) task = do
  TodoList $ tasks <> [task]

delTask :: TodoList -> TaskId -> TodoList
delTask (TodoList tasks) taskId = do
  TodoList $ filter (\task -> not $ task.taskId == taskId) tasks

getTasks :: TodoList -> [Task]
getTasks (TodoList tasks) = tasks

getTask :: TodoList -> TaskId -> Maybe Task
getTask (TodoList tasks) taskId = do
  case filter (\task -> task.taskId == taskId) tasks of
    [x] -> Just x
    _ -> Nothing

updateTask :: TodoList -> TaskId -> TaskUpdate -> TodoList
updateTask (TodoList tasks) taskId' TaskUpdate {..} = TodoList $ map update tasks
  where
    update task@Task {..} =
      if taskId == taskId'
        then Task taskId taskUpdateData taskDate taskUpdatePrio
        else task

initTodoState :: TodoList
initTodoState = TodoList $ [Task "1" "This is a demo task" date Medium]
  where
    date = parseTimeOrError False defaultTimeLocale "%F" "2022-10-19"

run :: IO ()
run = do
  appState <- newTVarIO initTodoState
  runServer $ todoApp appState
