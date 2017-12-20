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
import qualified Day15.Main
import qualified Day16.Main
import qualified Day17.Main
import qualified Day18.Main
import qualified Day19.Main
import qualified Day20.Main

solver :: Int -> IO ()
solver 1 = Day1.Main.main
solver 2 = Day2.Main.main
solver 3 = Day3.Main.main
solver 4 = Day4.Main.main
solver 5 = Day5.Main.main
solver 6 = Day6.Main.main
solver 7 = Day7.Main.main
solver 8 = Day8.Main.main
solver 9 = Day9.Main.main
solver 10 = Day10.Main.main
solver 11 = Day11.Main.main
solver 12 = Day12.Main.main
solver 13 = Day13.Main.main
solver 14 = Day14.Main.main
solver 15 = Day15.Main.main
solver 16 = Day16.Main.main
solver 17 = Day17.Main.main
solver 18 = Day18.Main.main
solver 19 = Day19.Main.main
solver 20 = Day20.Main.main
solver n = hPrint stderr $ "Unknown problem " ++ show n

main :: IO ()
main = do
  args <- getArgs
  solver . read . head $ args
