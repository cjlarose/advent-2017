module Day6.Main where

import Advent2017.Input (getInputAsString)
import Data.Set (member, insert, empty)
import qualified Data.Heap as Heap

type BankList = Heap.Heap (Int, Int)

dropBlocks :: BankList -> Int -> Int -> BankList
dropBlocks banks i n = Heap.map f banks
 where
  sz = Heap.size banks
  newBlocksAtDistance d = n `div` sz + (if d < n `mod` sz then 1 else 0)
  distance j = if j >= i then j - i else (j + sz) - i
  f (m, j) = (-((-m) + (newBlocksAtDistance . distance $ j)), j)

redistribute :: BankList -> BankList
redistribute banks = dropBlocks newHeap (succ i) (-n)
 where
  (n, i)  = Heap.minimum banks
  newHeap = Heap.insert (0, i) $ Heap.deleteMin banks

takeWhileDistinct :: Ord a => [a] -> [a]
takeWhileDistinct = f empty
 where
  f _ [] = []
  f seen (x:xs) | x `member` seen = []
                | otherwise       = x : f (insert x seen) xs

redistributeUntilLoopDetected :: BankList -> [BankList]
redistributeUntilLoopDetected = takeWhileDistinct . iterate redistribute

main :: IO ()
main = do
  input <- getInputAsString "6"
  let xs     = map read . words $ input :: [Int]
      banks  = Heap.fromList $ zipWith (\i c -> (-c, i)) [0 ..] xs
      states = redistributeUntilLoopDetected banks
  print . length $ states
  print . length . redistributeUntilLoopDetected . last $ states
