module Day2.Main where

import Data.List (tails)
import Data.Tuple (swap)
import Advent2017.Input (getInputAsString)

checksum :: [[Int]] -> Int
checksum rows = sum $ zipWith (-) (map maximum rows) (map minimum rows)

pairs :: [Int] -> [(Int, Int)]
pairs xs =
  concat $ zipWith (\x ts -> [ (x, y) | y <- ts ]) xs (tail . tails $ xs)

rowQuotient :: [Int] -> Int
rowQuotient xs =
  fst
    .  head
    .  filter (\(_, remainder) -> remainder == 0)
    .  map (uncurry divMod)
    $  pairs xs
    ++ map swap (pairs xs)

main :: IO ()
main = do
  input <- getInputAsString "2"
  let rows = map (map read . words) . lines $ input :: [[Int]]
  print . checksum $ rows
  print . sum . map rowQuotient $ rows
