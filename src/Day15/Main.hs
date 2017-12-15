module Day15.Main (solve) where

import Data.Bits ((.&.))

generator :: Int -> Int -> [Int]
generator factor = drop 1 . iterate (\x -> (x * factor) `mod` 0x7FFFFFFF)

part1 :: Int -> Int -> Int
part1 seedA seedB =
  length
    . filter (\(x, y) -> x .&. 0xFFFF == y .&. 0xFFFF)
    . take 40000000
    $ zip genA genB
 where
  genA = generator 16807 seedA
  genB = generator 48271 seedB

part2 :: Int -> Int -> Int
part2 seedA seedB =
  length
    . filter (\(x, y) -> x .&. 0xFFFF == y .&. 0xFFFF)
    . take 5000000
    $ zip genA genB
 where
  genA = filter (\x -> x `mod` 4 == 0) . generator 16807 $ seedA
  genB = filter (\x -> x `mod` 8 == 0) . generator 48271 $ seedB

solve :: String -> IO ()
solve input = do
  let (seedA:seedB:_) = map (read . last . words) . lines $ input
  print $ part1 seedA seedB
  print $ part2 seedA seedB
