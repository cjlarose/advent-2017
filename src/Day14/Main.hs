module Day14.Main (solve) where

import Day10.Main (knotHash)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.Bits (popCount, testBit)
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.IArray as IArray
import Data.List (foldl')

type Disk = UArray.UArray (Int, Int) Bool
type DisjointSetForest = IArray.Array (Int, Int) (Int, Int)

keys :: String -> [String]
keys prefix = map (\x -> prefix ++ "-" ++ show x) ([0 .. 127] :: [Int])

bsPopCount :: B.ByteString -> Int
bsPopCount = sum . map popCount . B.unpack

hashes :: String -> [B.ByteString]
hashes = map (knotHash 256 . fromString) . keys

numUsed :: String -> Int
numUsed = sum . map bsPopCount . hashes

bits :: B.ByteString -> [Bool]
bits = concatMap (\w -> reverse $ map (testBit w) [0 .. 7]) . B.unpack

makeDisk :: String -> Disk
makeDisk prefix = UArray.array ((0, 0), (127, 127)) . concat $ zipWith
  (\bs i -> zipWith (\b j -> ((i, j), b)) (bits bs) [0 ..])
  (hashes prefix)
  [0 ..]

showDisk :: Disk -> String
showDisk disk = unlines
  [ [ if disk UArray.! (i, j) then '.' else 'x' | j <- [0 .. 127] ]
  | i <- [0 .. 127]
  ]

makeForest :: Disk -> DisjointSetForest
makeForest disk = IArray.array ((0, 0), (127, 127)) elements
 where
  elements =
    map (\(c, b) -> if b then (c, c) else (c, (-1, -1))) . UArray.assocs $ disk

find :: (Int, Int) -> DisjointSetForest -> (Int, Int)
find x forest =
  let p = forest IArray.! x in if p == x then p else find p forest

union :: (Int, Int) -> (Int, Int) -> DisjointSetForest -> DisjointSetForest
union x y forest | find x forest == find y forest = forest
                 | otherwise = forest IArray.// [(find x forest, find y forest)]

numTrees :: DisjointSetForest -> Int
numTrees = length . filter (uncurry (==)) . IArray.assocs

numRegions :: Disk -> Int
numRegions disk = numTrees
  $ foldl' (\acc (x, y) -> union x y acc) (makeForest disk) pairs
 where
  neighbors :: (Int, Int) -> [(Int, Int)]
  neighbors (i, j) = [ (i - 1, j) | i > 0 ] ++ [ (i, j - 1) | j > 0 ]

  isUsed :: (Int, Int) -> Bool
  isUsed c = disk UArray.! c

  pairs :: [((Int, Int), (Int, Int))]
  pairs =
    concatMap
        ( \(c, b) ->
          if b then map (\n -> (c, n)) (filter isUsed . neighbors $ c) else []
        )
      . UArray.assocs
      $ disk

solve :: String -> IO ()
solve input = do
  let prefix = head . lines $ input
      disk   = makeDisk prefix
  print . numUsed $ prefix
  putStrLn . showDisk $ disk
  print . numRegions $ disk
