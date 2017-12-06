module Main where

import System.Environment (getArgs)
import qualified Day1.Main
import qualified Day2.Main
import qualified Day3.Main
import qualified Day4.Main
import qualified Day5.Main
import qualified Day6.Main

solver :: Int -> (String -> IO ())
solver 1 = Day1.Main.solve
solver 2 = Day2.Main.solve
solver 3 = Day3.Main.solve
solver 4 = Day4.Main.solve
solver 5 = Day5.Main.solve
solver 6 = Day6.Main.solve

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
