module Demo.Todo where

import Control.Monad.State
import Data.Set (fromList)
import Lucid (Html)
import Lucid.Html5
import Takoyaki.Engine
import Takoyaki.Htmx
import Prelude

mainW :: Widget
mainW =
  Widget
    { wId = "TodoMainW",
      wSwap = InnerHTML,
      wsEvent = const Nothing,
      wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger = Nothing,
      wChilds = fromList [todoInputFormW, todoListW]
    }
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender rs = pure $ do
      div_ [class_ "bg-gray-200"] $ do
        h1_ "Test"
        widgetRenderFromChildsStore "TodoListW" rs
        widgetRenderFromChildsStore "TodoInputFormW" rs

todoInputFormW :: Widget
todoInputFormW =
  Widget
    { wId = "TodoInputFormW",
      wSwap = InnerHTML,
      wsEvent = const Nothing,
      wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger = Nothing,
      wChilds = mempty
    }
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender _rs = pure $ do
      div_ [class_ "bg-gray-100"] $ do
        form_ [name_ "todo-input"] $ do
          label_ [for_ "task-name"] "Task name"
          input_ [name_ "task-name", type_ "text"]

todoListW :: Widget
todoListW =
  Widget
    { wId = "TodoListW",
      wSwap = InnerHTML,
      wsEvent = const Nothing,
      wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger = Nothing,
      wChilds = mempty
    }
  where
    wRender :: ChildsStore -> State (Maybe WState) (Html ())
    wRender _rs = pure $ do
      div_ [class_ "bg-gray-300"] "Empty Todo"

run :: IO ()
run = runServer "Takoyaki Todo Demo" widgets mainW
  where
    widgets = [mainW, todoInputFormW, todoListW]
