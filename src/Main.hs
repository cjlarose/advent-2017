module Main where

import System.Environment (getArgs)
import System.IO (hPrint, stderr)

import qualified Day1.Main
import qualified Day2.Main
import qualified Day3.Main
import qualified Day4.Main
import qualified Day5.Main
import qualified Day6.Main
import qualified Day7.Main
import qualified Day8.Main
import qualified Day9.Main
import qualified Day10.Main
import qualified Day11.Main
import qualified Day12.Main
import qualified Day13.Main
import qualified Day14.Main

solver :: Int -> (String -> IO ())
solver 1 = Day1.Main.solve
solver 2 = Day2.Main.solve
solver 3 = Day3.Main.solve
solver 4 = Day4.Main.solve
solver 5 = Day5.Main.solve
solver 6 = Day6.Main.solve
solver 7 = Day7.Main.solve
solver 8 = Day8.Main.solve
solver 9 = Day9.Main.solve
solver 10 = Day10.Main.solve
solver 11 = Day11.Main.solve
solver 12 = Day12.Main.solve
solver 13 = Day13.Main.solve
solver 14 = Day14.Main.solve
solver n = const (hPrint stderr $ "Unknown problem " ++ show n)

main :: IO ()
main = do
  args <- getArgs
  let problemNumber = head args
  contents <- readFile $ "inputs/" ++ problemNumber ++ ".txt"
  solver (read problemNumber) contents
