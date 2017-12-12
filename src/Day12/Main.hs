module Day12.Main (solve) where

import qualified Data.Map.Strict as Map
import qualified Data.IntSet as Set
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

connectedSubgraph :: Int -> Graph -> [Int]
connectedSubgraph v g = go g Set.empty v
 where
  go :: Graph -> Set.IntSet -> Int -> [Int]
  go adj visited u
    | Set.member u visited = []
    | otherwise = u : concatMap (go adj (Set.insert u visited))
                                (fromMaybe [] (Map.lookup u adj))

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly adjacencyList . pack $ input
  case parsed of
    Left  err -> print err
    Right ast -> do
      let graph = Map.fromList ast
      print . length . connectedSubgraph 0 $ graph
