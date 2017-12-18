module Day17.Main where

import Advent2017.Input (getFirstLineOfInput)
import qualified Data.Sequence as Seq
import Data.Maybe (fromJust)

buffers :: Int -> [Seq.Seq Int]
buffers k = map (\(_, _, s) -> s) $ iterate buffers' (1, 0, Seq.singleton 0)
 where
  buffers' (n, pos, xs) =
    let newPos = (((pos + k) `mod` n) + 1)
    in  (n + 1, newPos, Seq.insertAt newPos n xs)

valueAfter :: Int -> Seq.Seq Int -> Int
valueAfter marker s = fromJust
  $ Seq.lookup (fromJust (Seq.elemIndexL marker s) + 1 `mod` Seq.length s) s

part1 :: Int -> Int
part1 k = valueAfter 2017 (buffers k !! 2017)

part2 :: Int -> Int
part2 k = valueAfter 0 (buffers k !! 50000000)

main :: IO ()
main = do
  n <- read <$> getFirstLineOfInput "17"
  print . part1 $ n
  print . part2 $ n
