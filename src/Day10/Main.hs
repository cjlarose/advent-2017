module Day10.Main (solve) where

import Data.List (foldl')
import Data.List.Split (splitOn)

data KnotState = KnotState { skipSize :: Int
                           , skipCount :: Int
                           , marks :: [Int] } deriving Show

freshLoop :: Int -> KnotState
freshLoop n = KnotState
  { skipSize  = 0
  , marks     = [0 .. n]
  , skipCount = 0
  }

advance :: Int -> ([Int] -> [Int]) -> [Int] -> [Int]
advance k f xs = let (start, rest) = splitAt k xs in rest ++ f start

twist :: KnotState -> Int -> KnotState
twist s n = KnotState
  { skipSize  = 1 + skipSize s
  , marks     = advance (skipSize s) id . advance n reverse $ marks s
  , skipCount = skipCount s + skipSize s + n
  }

knotHash :: Int -> [Int] -> Int
knotHash n xs = x * y
 where
  final   = foldl' twist (freshLoop n) xs
  (x:y:_) = drop (n + 1 - (skipCount final `mod` (n + 1))) (marks final)

solve :: String -> IO ()
solve input = do
  let lengths = map read $ splitOn "," input
  print . knotHash 255 $ lengths
