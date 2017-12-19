module Day19.Main (main) where

import Advent2017.Input (getInputAsString)
import Data.Char (isAlpha, isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Array.IArray as Array
import Data.Array.IArray ((!))

type Grid = Array.Array (Int, Int) Char
type Direction = (Int, Int)

toGrid :: String -> Grid
toGrid str = Array.array ((0, 0), (length rows - 1, length (head rows) - 1))
                         entries
 where
  rows    = lines str
  entries = concat
    $ zipWith (\row i -> zipWith (\c j -> ((i, j), c)) row [0 ..]) rows [0 ..]

findLetters :: Grid -> Map.Map (Int, Int) Char
findLetters = Map.fromList . filter (isAlpha . snd) . Array.assocs

findPluses :: Grid -> Set.Set (Int, Int)
findPluses = Set.fromList . map fst . filter ((=='+') . snd) . Array.assocs

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (a, b) (c, d) = (a + c, b + d)

options :: Grid -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
options g pos going =
  map fst
    . filter (\(_, coord) -> not . isSpace $ g ! coord)
    . map (\dir -> (dir, move pos dir))
    $ dirs
 where
  opposite (di, dj) = (-1 * di, -1 * dj)
  dirs = filter (/=opposite going) [(-1, 0), (0, 1), (1, 0), (0, -1)]

nextDir :: Grid -> (Int, Int) -> (Int, Int) -> (Int, Int)
nextDir g pos going = head $ options g pos going

routePacket
  :: (Int, Int)
  -> Grid
  -> Map.Map (Int, Int) Char
  -> Set.Set (Int, Int)
  -> Direction
  -> String
routePacket pos g letters pluses dir = go
 where
  continue newDir = routePacket (move pos newDir) g letters pluses newDir
  go | Map.member pos letters   = (letters Map.! pos) : continue dir
     | Set.member pos pluses    = continue (nextDir g pos dir)
     | null (options g pos dir) = []
     | otherwise                = continue dir

getStartPos :: Grid -> (Int, Int)
getStartPos = fst . head . filter (\(_, c) -> not . isSpace $ c) . Array.assocs

part1 :: Grid -> String
part1 g = routePacket (getStartPos g) g (findLetters g) (findPluses g) (1, 0)

main :: IO ()
main = do
  input <- getInputAsString "19"
  let grid = toGrid input
  putStrLn . part1 $ grid
