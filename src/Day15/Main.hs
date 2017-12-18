module Day15.Main where

import Advent2017.Input (getInputAsString)
import Data.Bits ((.&.))

generator :: Int -> Int -> [Int]
generator factor = drop 1 . iterate (\x -> (x * factor) `mod` 0x7FFFFFFF)

judge :: Int -> Int -> Bool
judge x y = x .&. 0xFFFF == y .&. 0xFFFF

countMatches :: Int -> [Int] -> [Int] -> Int
countMatches n genA genB =
  length . filter (uncurry judge) . take n $ zip genA genB

part1 :: Int -> Int -> Int
part1 seedA seedB =
  countMatches 40000000 (generator 16807 seedA) (generator 48271 seedB)

part2 :: Int -> Int -> Int
part2 seedA seedB = countMatches 5000000 genA genB
 where
  genA = filter (\x -> x `mod` 4 == 0) . generator 16807 $ seedA
  genB = filter (\x -> x `mod` 8 == 0) . generator 48271 $ seedB

main :: IO ()
main = do
  input <- getInputAsString "15"
  let (seedA:seedB:_) = map (read . last . words) . lines $ input
  print $ part1 seedA seedB
  print $ part2 seedA seedB
