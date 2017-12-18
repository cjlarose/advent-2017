module Day3.Main where

import Control.Arrow ((***))
import Advent2017.Input (getInputAsString)

distance :: Int -> Int
distance x = abs (((x - (n * n) - 1) `mod` (n + 1)) - dv) + n - dv
 where
  odds = [ z * 2 + 1 | z <- [0 ..] ]
  n    = (head . filter (\y -> (y * y) > x) $ odds) - 2
  dv   = (n - 1) `div` 2

nextCoord :: (Int, Int) -> (Int, Int)
nextCoord (x, y) | y <= -x && y <= x = (x + 1, y)
                 | y > -x && y < x   = (x, y + 1)
                 | y >= x && y > -x  = (x - 1, y)
                 | otherwise         = (x, y - 1)

coordSpiral :: [(Int, Int)]
coordSpiral = iterate nextCoord (0, 0)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = map ((+) x *** (+) y) . take 8 . drop 1 $ coordSpiral

spiralSums' :: [(Int, Int)] -> ((Int, Int) -> Int) -> [Int]
spiralSums' [] _ = error "Whoops"
spiralSums' (x:xs) valueAtCoord = newVal : spiralSums' xs newF
 where
  newVal = sum . map valueAtCoord . neighbors $ x
  newF c = if c == x then newVal else valueAtCoord c

spiralSums :: [Int]
spiralSums = spiralSums' (drop 1 coordSpiral) (fromEnum . (==(0, 0)))

main :: IO ()
main = do
  input <- getInputAsString "3"
  let x = read input :: Int
  print . distance $ x
  print . head . filter (>x) $ spiralSums
