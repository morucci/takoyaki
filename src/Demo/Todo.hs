module Demo.Todo where

import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable (hash))
import qualified Data.Map as Map
import Data.Set (fromList)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Lucid (Html, ToHtml (toHtml))
import Lucid.Html5
import Takoyaki.Engine
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
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTId == "InputFormSubmitted" = do
          case Map.lookup ("task-name") e.wseData of
            Just (Just task) -> Just $ WEvent "TodoListW" "AddTask" (Aeson.String task)
            _ -> Nothing
      | otherwise = Nothing
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
    wState = Just $ setTodoListAsJSON emptyTodoList
    wsEvent :: WSEvent -> Maybe WEvent
    wsEvent e
      | e.wseTId == "DelTask" = do
          case Map.lookup ("TaskId") e.wseData of
            Just (Just taskId) -> do
              Just $ WEvent "TodoListW" "DelTask" (Aeson.String taskId)
            _ -> Nothing
      | otherwise = Nothing
    wStateUpdate :: WEvent -> State (Maybe WState) ()
    wStateUpdate e = do
      ws <- get
      case join $ getTodoListFromJSON <$> ws of
        Just todoList -> do
          case e of
            WEvent _ "AddTask" (Aeson.String taskContent) -> do
              put . Just . setTodoListAsJSON $ addTask todoList taskContent
            WEvent _ "DelTask" (Aeson.String taskId) ->
              put $ Just $ setTodoListAsJSON $ delTask todoList taskId
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
                  \t -> li_ $ renderTask t.taskId t.taskData
        _otherwise -> error "Unexpected widget state"
      where
        renderTask :: TodoTaskId -> Text -> Html ()
        renderTask taskId taskData = span_ $ do
          withEvent "DelTask" Nothing [("TaskId", taskId)] $ do
            button_ [class_ "mr-2 bg-red-600"] "Del"
            toHtml taskData

type TodoTaskId = Text

data Task = Task
  { taskId :: TodoTaskId,
    taskData :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON Task

instance ToJSON Task

instance Hashable Task

data TodoList = TodoList [Task] deriving (Show, Generic, Eq)

instance FromJSON TodoList

instance ToJSON TodoList

instance Hashable TodoList

getTodoListFromJSON :: Aeson.Value -> Maybe TodoList
getTodoListFromJSON v = case Aeson.fromJSON v of
  Aeson.Success todo -> Just todo
  Aeson.Error _ -> Nothing

setTodoListAsJSON :: TodoList -> Aeson.Value
setTodoListAsJSON = Aeson.toJSON

emptyTodoList :: TodoList
emptyTodoList = TodoList mempty

addTask :: TodoList -> Text -> TodoList
addTask todo@(TodoList tasks) taskContent = do
  let task = Task (pack . show $ hash todo) taskContent
  TodoList $ tasks <> [task]

delTask :: TodoList -> TodoTaskId -> TodoList
delTask (TodoList tasks) taskId = TodoList $ filter (\t -> not $ t.taskId == taskId) tasks

run :: IO ()
run = runServer "Takoyaki Todo Demo" widgets mainW
  where
    widgets = [mainW, todoInputFormW, todoListW]
