module Demo.Todo where

import Control.Monad.State
import Data.Set (fromList)
import Lucid (Html)
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
  (mkWidget "TodoInputFormW") {wRender = const wRender}
  where
    wRender :: State (Maybe WState) (Html ())
    wRender = pure $ do
      div_ [class_ "bg-gray-100"] $ do
        form_ [name_ "todo-input"] $ do
          label_ [for_ "task-name"] "Task name"
          input_ [name_ "task-name", type_ "text"]

todoListW :: Widget
todoListW = (mkWidget "TodoListW") {wRender}
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender _rs = pure $ do
      div_ [class_ "bg-gray-300"] "Empty Todo"

run :: IO ()
run = runServer "Takoyaki Todo Demo" widgets mainW
  where
    widgets = [mainW, todoInputFormW, todoListW]
