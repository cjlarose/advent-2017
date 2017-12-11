module Day11.Main (solve) where

import Data.List (foldl')
import Data.List.Split (splitOn)

data Dir = N | NE | SE | S | SW | NW

toDir :: String -> Dir
toDir "n"  = N
toDir "ne" = NE
toDir "se" = SE
toDir "s"  = S
toDir "sw" = SW
toDir "nw" = NW
toDir s    = error $ "Unknown direction " ++ s

move :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
move (x, y, z) N  = (x, y + 1, z - 1)
move (x, y, z) S  = (x, y - 1, z + 1)
move (x, y, z) NE = (x + 1, y, z - 1)
move (x, y, z) SW = (x - 1, y, z + 1)
move (x, y, z) NW = (x - 1, y + 1, z)
move (x, y, z) SE = (x + 1, y - 1, z)

followPath :: (Int, Int, Int) -> [Dir] -> (Int, Int, Int)
followPath = foldl' move

distance :: (Int, Int, Int) -> Int
distance (x, y, z) = sum (map abs [x, y, z]) `div` 2

solve :: String -> IO ()
solve input = do
  let dirs = map toDir . splitOn "," . head . lines $ input
  print . distance $ followPath (0, 0, 0) dirs
