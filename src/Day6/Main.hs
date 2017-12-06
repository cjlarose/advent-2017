module Day6.Main where

import Data.Set (Set, member, insert, empty)
import qualified Data.Heap as Heap

type BankList = Heap.Heap (Int, Int)

update :: (Int -> Int) -> Int -> BankList -> BankList
update f i =
  Heap.map (\x@(n, j) -> if j == i then (negate . f . negate $ n, i) else x)

dropBlocks :: BankList -> Int -> Int -> BankList
dropBlocks banks _ 0 = banks
dropBlocks banks i n = dropBlocks newBanks (succ i) (pred n)
 where
  j        = i `mod` Heap.size banks
  newBanks = update succ j banks

redistribute :: BankList -> BankList
redistribute banks = dropBlocks newHeap (succ i) (-n)
 where
  (n, i)  = Heap.minimum banks
  newHeap = Heap.insert (0, i) $ Heap.deleteMin banks

takeWhileDistinct :: Ord a => [a] -> [a]
takeWhileDistinct = f empty
 where
  f seen (x:xs) | x `member` seen = []
                | otherwise       = x : f (insert x seen) xs

redistributeUntilLoopDetected :: BankList -> [BankList]
redistributeUntilLoopDetected = takeWhileDistinct . iterate redistribute

solve :: String -> IO ()
solve input = do
  let xs       = map read . words $ input :: [Int]
      maxIndex = length xs - 1
      banks    = Heap.fromList $ zipWith (\i c -> (-c, i)) [0 ..] xs
      states   = redistributeUntilLoopDetected banks
  print . length $ states
  print . length . redistributeUntilLoopDetected . last $ states
