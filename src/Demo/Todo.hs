module Demo.Todo where

import Control.Monad.State
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Set (fromList)
import qualified Data.Vector as V
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
        h1_ "Test"
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
        withEvent "InputFormSubmitted" Nothing $ do
          form_ [name_ "todo-input"] $ do
            label_ [for_ "task-name"] "Task name"
            input_ [name_ "task-name", type_ "text"]
            button_ [type_ "submit"] "Add task"

todoListW :: Widget
todoListW = (mkWidget "TodoListW") {wRender, wState, wStateUpdate}
  where
    wState = Just (Aeson.Array $ V.fromList [])
    wStateUpdate :: WEvent -> State (Maybe WState) ()
    wStateUpdate e = do
      ws <- get
      case ws of
        Just (Aeson.Array entries) -> do
          case extractAddTaskEvent e of
            Just task -> put $ Just (Aeson.Array (entries <> V.singleton task))
            Nothing -> pure ()
        _otherwise -> pure ()
      where
        extractAddTaskEvent (WEvent _ "AddTask" task) = Just task
        extractAddTaskEvent _ = Nothing

    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender _rs = do
      ws <- get
      case ws of
        Just (Aeson.Array entries)
          | V.null entries -> do
              pure $ div_ [class_ "bg-gray-300"] "Empty Todo"
          | not $ V.null entries -> do
              pure $ div_ [class_ "bg-gray-300"] $ do
                ul_ $ forM_ entries $ do \entry -> li_ $ renderTask entry
        _otherwise -> error "Unexpected widget state"
      where
        renderTask :: Aeson.Value -> Html ()
        renderTask (Aeson.String task) = li_ $ toHtml task
        renderTask _ = pure ()

run :: IO ()
run = runServer "Takoyaki Todo Demo" widgets mainW
  where
    widgets = [mainW, todoInputFormW, todoListW]
