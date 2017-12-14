module Day14.Main (solve) where

import Day10.Main (knotHash)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.Bits (popCount)

keys :: String -> [String]
keys prefix = map (\x -> prefix ++ "-" ++ show x) ([0 .. 127] :: [Int])

bsPopCount :: B.ByteString -> Int
bsPopCount = sum . map popCount . B.unpack

numUsed :: String -> Int
numUsed prefix = sum . map bsPopCount $ hashes
  where
    hashes = map (knotHash 256 . fromString) (keys prefix)

solve :: String -> IO ()
solve input = do
  let prefix = head . lines $ input
  print . numUsed $ prefix
