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

part1 :: Int -> Int
part1 k = fromJust
  $ Seq.lookup (fromJust (Seq.elemIndexL 2017 s) + 1 `mod` Seq.length s) s
  where s = buffers k !! 2017

main :: IO ()
main = do
  n <- read <$> getFirstLineOfInput "17"
  print . part1 $ n
