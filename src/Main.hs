module Main where

import System.Environment (getArgs)
import qualified Day1.Main
import qualified Day2.Main
import qualified Day3.Main

solver :: Int -> (String -> IO ())
solver 1 = Day1.Main.solve
solver 2 = Day2.Main.solve
solver 3 = Day3.Main.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
