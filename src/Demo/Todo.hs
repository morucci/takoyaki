module Demo.Todo where

import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable (hash))
import qualified Data.Map as Map
import Data.Set (fromList)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Lucid (Html, ToHtml (toHtml))
import Lucid.Html5
import Takoyaki.Engine
import Witch
import Prelude

mainW :: Widget
mainW =
  (mkWidget "TodoMainW")
    { wRender,
      wChilds = fromList [todoInputFormW, todoListW]
    }
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender cs = pure $ do
      div_ [class_ "bg-gray-200"] $ do
        h1_ "Takoyaki TODO list demo"
        renderW cs "TodoListW"
        renderW cs "TodoInputFormW"

todoInputFormW :: Widget
todoInputFormW =
  (mkWidget "TodoInputFormW") {wRender = const wRender, wsEvent}
  where
    wsEvent :: WSEvent -> Maybe (IO WEvent)
    wsEvent e
      | e.wseTId == "InputFormSubmitted" = do
          case Map.lookup ("task-name") e.wseData of
            Just (Just taskContent) -> Just $ genAddTaskEvent taskContent
            _ -> Nothing
      | otherwise = Nothing
      where
        genAddTaskEvent :: Text -> IO (WEvent)
        genAddTaskEvent taskContent = do
          now <- getCurrentTime
          let task = Task taskContent now
          pure $ WEvent "TodoListW" "AddTask" (Aeson.toJSON task)
    wRender :: State (Maybe WState) (Html ())
    wRender = pure $ do
      div_ [class_ "bg-gray-100"] $ do
        withEvent "InputFormSubmitted" Nothing [] $ do
          form_ [name_ "todo-input"] $ do
            label_ [for_ "task-name"] "Task name"
            input_ [name_ "task-name", type_ "text"]
            button_ [type_ "submit"] "Add task"

todoListW :: Widget
todoListW =
  (mkWidget "TodoListW")
    { wRender = const wRender,
      wState,
      wStateUpdate,
      wsEvent
    }
  where
    wState = Just $ Aeson.toJSON emptyTodoList
    wsEvent :: WSEvent -> Maybe (IO WEvent)
    wsEvent e
      | e.wseTId == "DelTask" = do
          case Map.lookup ("TaskId") e.wseData of
            Just (Just taskId) -> do
              Just . pure $ WEvent "TodoListW" "DelTask" (Aeson.String taskId)
            _ -> Nothing
      | otherwise = Nothing
    wStateUpdate :: WEvent -> State (Maybe WState) ()
    wStateUpdate e = do
      ws <- get
      case join $ getTodoListFromJSON <$> ws of
        Just todoList -> do
          case e of
            WEvent _ "AddTask" jsonTask -> do
              let taskM = getTaskFromJSON jsonTask
              case taskM of
                Just task -> put . Just . Aeson.toJSON $ addTask todoList task
                _ -> pure ()
            WEvent _ "DelTask" (Aeson.String taskId) ->
              put . Just . Aeson.toJSON $ delTask todoList taskId
            _ -> pure ()
        _otherwise -> pure ()

    wRender :: State (Maybe WState) (Html ())
    wRender = do
      ws <- get
      case join $ getTodoListFromJSON <$> ws of
        Just (TodoList tasks)
          | null tasks -> do
              pure $ div_ [class_ "bg-gray-300"] "Empty Todo"
          | otherwise -> do
              pure $ div_ [class_ "bg-gray-300"] $ do
                ul_ $ forM_ tasks $ do
                  \(tId, task) -> li_ $ renderTask tId task
        _otherwise -> error "Unexpected widget state"
      where
        renderTask :: TodoTaskId -> Task -> Html ()
        renderTask taskId task = span_ $ do
          withEvent "DelTask" Nothing [("TaskId", taskId)] $ do
            button_ [class_ "mr-2 bg-red-600"] "Del"
            span_ [class_ "mr-2"] $ toHtml $ show task.taskDate
            span_ [class_ "mr-2"] $ toHtml task.taskData

type TodoTaskId = Text

data Task = Task
  { taskData :: Text,
    taskDate :: UTCTime
  }
  deriving (Show, Generic, Eq)

instance FromJSON Task

instance ToJSON Task

instance Hashable Task

data TodoList = TodoList [(TodoTaskId, Task)] deriving (Show, Generic, Eq)

instance FromJSON TodoList

instance ToJSON TodoList

instance Hashable TodoList

getFromJSON :: FromJSON a => Aeson.Value -> Maybe a
getFromJSON jsonV = case Aeson.fromJSON jsonV of
  Aeson.Success v -> Just v
  Aeson.Error _ -> Nothing

getTaskFromJSON :: Aeson.Value -> Maybe Task
getTaskFromJSON = getFromJSON

getTodoListFromJSON :: Aeson.Value -> Maybe TodoList
getTodoListFromJSON = getFromJSON

emptyTodoList :: TodoList
emptyTodoList = TodoList mempty

addTask :: TodoList -> Task -> TodoList
addTask todo@(TodoList tasks) task = do
  let taskId = (from . show $ hash todo)
  TodoList $ tasks <> [(taskId, task)]

delTask :: TodoList -> TodoTaskId -> TodoList
delTask (TodoList tasks) taskId = TodoList $ filter (\(tId, _) -> not $ tId == taskId) tasks

run :: IO ()
run = runServer "Takoyaki Todo Demo" widgets mainW
  where
    widgets = [mainW, todoInputFormW, todoListW]
