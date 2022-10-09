module Demo.Todo where

import Control.Concurrent.STM
import Control.Monad.State
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
      wTrigger = Nothing
    }
  where
    wRender :: State (Maybe WState) (Html ())
    -- wRender = pure $ do
    wRender = pure $ pure ()

-- div_ [class_ "bg-gray-200"] $ do
--   h1_ "Test"
--   renderWidget undefined "TodoInputFormW"

todoInputFormW :: Widget
todoInputFormW =
  Widget
    { wId = "TodoInputFormW",
      wSwap = InnerHTML,
      wsEvent = const Nothing,
      wRender,
      wState = Nothing,
      wStateUpdate = const $ pure (),
      wTrigger = Nothing
    }
  where
    wRender :: State (Maybe WState) (Html ())
    wRender = pure $ do
      div_ [class_ "bg-gray-100"] $ do
        form_ [name_ "todo-input"] $ do
          label_ [for_ "task-name"] "Task name"
          input_ [name_ "task-name", type_ "text"]

run :: IO ()
run = runServer "Takoyaki Todo Demo" widget initDom
  where
    widget = [todoMainW, todoInputFormW]
    initDom :: Registry -> STM (Html ())
    initDom registry = do
      todoMainW' <- renderWidget registry "TodoMainW"
      pure $ div_ [id_ "my-dom"] $ todoMainW'
