module Main where

import System.Environment (getArgs)
import qualified Day1.Main

solver :: Int -> (String -> IO ())
solver 1 = Day1.Main.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
