module Main (main) where

import qualified Demo.MineSweeper (run)
import qualified Demo.Todo (run)
import System.Environment
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["MineSweeper"] -> Demo.MineSweeper.run 8092
    ["TodoList"] -> Demo.Todo.run 8092
    _ -> putStrLn "Unknown app. Exit"
