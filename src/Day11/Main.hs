module Day11.Main (solve) where

import Data.List.Split (splitOn)
import Data.Char (toUpper)

data Dir = N | NE | SE | S | SW | NW deriving Read

move :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
move (x, y, z) N  = (x, y + 1, z - 1)
move (x, y, z) S  = (x, y - 1, z + 1)
move (x, y, z) NE = (x + 1, y, z - 1)
move (x, y, z) SW = (x - 1, y, z + 1)
move (x, y, z) NW = (x - 1, y + 1, z)
move (x, y, z) SE = (x + 1, y - 1, z)

followPath :: (Int, Int, Int) -> [Dir] -> [(Int, Int, Int)]
followPath _ [] = []
followPath pos (dir:xs) =
  let newPos = move pos dir in newPos : followPath newPos xs

distance :: (Int, Int, Int) -> Int
distance (x, y, z) = sum (map abs [x, y, z]) `div` 2

solve :: String -> IO ()
solve input = do
  let dirs = map read . splitOn "," . (map toUpper) . head . lines $ input
      path = followPath (0, 0, 0) dirs
  print . distance . last $ path
  print . maximum . map distance $ path
