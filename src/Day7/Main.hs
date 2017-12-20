module Day7.Main where

import Advent2017.Input (getInputAsText)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (partition)
import Data.Attoparsec.Text (Parser, takeWhile1, inClass, parseOnly, string, option, sepBy1, sepBy, endOfInput, decimal, char)
import Data.Text (Text, pack, unpack)

programName :: Parser String
programName = unpack <$> takeWhile1 (inClass "a-z")

weight :: Parser Int
weight = decimal

arrow :: Parser Text
arrow = string . pack $ " -> "

dependantsList :: Parser [String]
dependantsList = programName `sepBy1` string (pack ", ")

programDescription :: Parser (String, Int, [String])
programDescription =
  (\a b c -> (a, b, c))
    <$> (programName <* char ' ')
    <*> (char '(' *> weight <* char ')')
    <*> option [] (arrow *> dependantsList)

programList :: Parser [(String, Int, [String])]
programList = programDescription `sepBy` char '\n' <* char '\n' <* endOfInput

type WeightMap = Map.Map String Int
type AdjacencyList = [(String, String)]

childrenPrograms :: AdjacencyList -> String -> [String]
childrenPrograms adj prog = map snd . filter ((== prog) . fst) $ adj

progWeight :: WeightMap -> AdjacencyList -> String -> Int
progWeight w adj prog =
  (w Map.! prog) + (sum . map (progWeight w adj) $ childrenPrograms adj prog)

findProgram :: WeightMap -> AdjacencyList -> String -> (String, Int)
findProgram w adj root = findProgram' w adj root 0

findProgram' :: WeightMap -> AdjacencyList -> String -> Int -> (String, Int)
findProgram' w adj root siblingWeight = if null bad
  then (root, siblingWeight - (sum . map getWeight $ children))
  else findProgram' w adj (head bad) (getWeight (head good))
 where
  children    = childrenPrograms adj root
  getWeight   = progWeight w adj
  (bad, good) = partition
    (\p -> notElem (getWeight p) . map getWeight . filter (/=p) $ children)
    children

main :: IO ()
main = do
  parsed <- parseOnly programList <$> getInputAsText "7"
  case parsed of
    Left  err   -> print err
    Right progs -> do
      let progNames  = Set.fromList $ map (\(name, _, _) -> name) progs
          dependants = Set.fromList $ concatMap (\(_, _, deps) -> deps) progs
          root       = Set.findMin $ Set.difference progNames dependants
          adjList    = concatMap (\(p, _, cs) -> [ (p, c) | c <- cs ]) progs
          weights    = Map.fromList $ map (\(name, w, _) -> (name, w)) progs
      print root
      print . findProgram weights adjList $ root
