module Day10.Main (solve, knotHash) where

import Data.List (foldl')
import Data.List.Split (splitOn, chunksOf)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Word (Word8)
import Data.Bits (xor)
import Data.Array.Unboxed (Array, listArray, (!), (//), elems)

data KnotState = KnotState { loopSize :: Int
                           , skipSize :: Int
                           , pos :: Int
                           , marks :: Array Int Word8 } deriving Show

freshLoop :: Int -> KnotState
freshLoop n = KnotState
  { loopSize = n
  , skipSize = 0
  , pos      = 0
  , marks    = listArray (0, n - 1) . map fromIntegral $ [0 .. (n - 1)]
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

knotHashRound :: [Int] -> KnotState -> KnotState
knotHashRound xs s = foldl' twist s xs

sparseHash :: Int -> [Int] -> [Word8]
sparseHash n xs =
  elems . marks $ iterate (knotHashRound xs) (freshLoop n) !! 64

condenseHash :: [Word8] -> [Word8]
condenseHash = map (foldl' xor 0) . chunksOf 16

knotHash :: Int -> B.ByteString -> B.ByteString
knotHash n input = B.pack . condenseHash . sparseHash n $ lengths
  where lengths = (map fromIntegral . B.unpack $ input) ++ [17, 31, 73, 47, 23]

part1 :: String -> Int
part1 input = fromIntegral (final ! 0) * fromIntegral (final ! 1)
 where
  lengths = map read . splitOn "," $ input
  final   = marks $ knotHashRound lengths (freshLoop 256)

part2 :: String -> String
part2 input = toString . toLazyByteString . byteStringHex $ bytes
  where bytes = knotHash 256 (fromString . head . lines $ input)

solve :: String -> IO ()
solve input = do
  print . part1 $ input
  putStrLn . part2 $ input
