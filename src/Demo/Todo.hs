module Demo.Todo where

import Control.Concurrent.STM
import Control.Monad.State
import Data.Set (fromList)
import Lucid (Html)
import Lucid.Html5
import Takoyaki.Engine
import Takoyaki.Htmx
import Prelude

todoMainW :: Widget
todoMainW =
  Widget
    { wId = "TodoMainW",
      wSwap = InnerHTML,
      wsEvent = const Nothing,
      wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger = Nothing,
      wChildWidget = fromList [todoInputFormW.wId, todoListW.wId]
    }
  where
    wRender :: RStore -> State (Maybe WState) (Html ())
    wRender rs = pure $ do
      div_ [class_ "bg-gray-200"] $ do
        h1_ "Test"
        widgetRenderFromRStore "TodoListW" rs
        widgetRenderFromRStore "TodoInputFormW" rs

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
      wChildWidget = mempty
    }
  where
    wRender :: RStore -> State (Maybe WState) (Html ())
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
      wChildWidget = mempty
    }
  where
    wRender :: RStore -> State (Maybe WState) (Html ())
    wRender _rs = pure $ do
      div_ [class_ "bg-gray-300"] "Empty Todo"

run :: IO ()
run = runServer "Takoyaki Todo Demo" widget initDom
  where
    widget = [todoMainW, todoInputFormW, todoListW]
    initDom :: Registry -> STM (Html ())
    initDom registry = do
      rs <- mkRStore registry (todoMainW.wChildWidget)
      let todoMainW' = widgetRender rs todoMainW
      pure $ div_ [id_ "my-dom"] $ todoMainW'
