module Day12.Main where

import Advent2017.Input (getInputAsString)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Attoparsec.Text (Parser, parseOnly, sepBy, endOfLine, endOfInput, decimal, string)
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)

type Graph = Map.Map Int [Int]

adjacency :: Parser (Int, [Int])
adjacency = liftA2 (,)
                   (decimal <* string (pack " <-> "))
                   (decimal `sepBy` string (pack ", "))

adjacencyList :: Parser [(Int, [Int])]
adjacencyList = (adjacency `sepBy` endOfLine) <* endOfLine <* endOfInput

connectedSubgraph :: Int -> Graph -> IntSet.IntSet
connectedSubgraph v g = go g IntSet.empty v
 where
  go :: Graph -> IntSet.IntSet -> Int -> IntSet.IntSet
  go adj visited u
    | IntSet.member u visited
    = IntSet.empty
    | otherwise
    = foldl' (\acc x -> IntSet.union acc $ go adj acc x)
             (IntSet.insert u visited)
      $ fromMaybe [] (Map.lookup u adj)

allSubgraphs :: Graph -> Set.Set IntSet.IntSet
allSubgraphs graph =
  let allComponents = map (`connectedSubgraph`graph) $ Map.keys graph
  in  foldl' Set.union Set.empty $ map Set.singleton allComponents

main :: IO ()
main = do
  input <- getInputAsString "12"
  let parsed = parseOnly adjacencyList . pack $ input
  case parsed of
    Left  err -> print err
    Right ast -> do
      let graph = Map.fromList ast
      print . IntSet.size . connectedSubgraph 0 $ graph
      print . Set.size . allSubgraphs $ graph
