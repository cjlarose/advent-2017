module Day10.Main (solve) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Array.Unboxed (Array, listArray, (!), (//))

data KnotState = KnotState { loopSize :: Int
                           , skipSize :: Int
                           , pos :: Int
                           , marks :: Array Int Int } deriving Show

freshLoop :: Int -> KnotState
freshLoop n = KnotState
  { loopSize = n
  , skipSize = 0
  , pos      = 0
  , marks    = listArray (0, n - 1) [0 .. (n - 1)]
  }

twist :: KnotState -> Int -> KnotState
twist s k = s { skipSize = 1 + skipSize s
              , pos      = (pos s + skipSize s + k) `mod` loopSize s
              , marks    = marks s // changes
              }
 where
  indices   = take k $ iterate (\x -> (x + 1) `mod` loopSize s) (pos s)
  newValues = reverse $ map (\i -> marks s ! i) indices
  changes   = zip indices newValues

knotHash :: Int -> [Int] -> Int
knotHash n xs = (final ! 0) * (final ! 1)
  where final = marks $ foldl' twist (freshLoop n) xs

solve :: String -> IO ()
solve input = do
  let lengths = map read $ splitOn "," input
  print . knotHash 256 $ lengths
