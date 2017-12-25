module Day24.Main (main) where

import Data.Attoparsec.Text (Parser, parseOnly, sepBy, endOfLine, decimal, endOfInput, char)
import Control.Applicative (liftA2)
import qualified Data.Set as Set
import Data.List (foldl')
import Advent2017.Input (getInputAsText)
import Control.Arrow ((&&&))

type Component = (Int, Int)
type ComponentBag = Set.Set Component
type Bridge = [Component]

component :: Parser Component
component = liftA2 (,) decimal (char '/' *> decimal)

components :: Parser [Component]
components = component `sepBy` endOfLine <* endOfLine <* endOfInput

getComponents :: IO ComponentBag
getComponents =
  Set.fromList . either (const []) id . parseOnly components <$> getInputAsText
    "24"

bridges :: ComponentBag -> Int -> [Bridge]
bridges available startPort = l ++ r
 where
  options = Set.toList available
  f c p = [c] : map (\b -> c : b) (bridges (Set.delete c available) p)
  l = concatMap (\c@(_, p) -> f c p) . filter ((==startPort) . fst) $ options
  r = concatMap (\c@(p, _) -> f c p) . filter ((==startPort) . snd) $ options

strength :: Bridge -> Int
strength = foldl' (\acc (x, y) -> x + y + acc) 0

part1 :: ComponentBag -> Int
part1 xs = maximum . map strength $ bridges xs 0

part2 :: ComponentBag -> Int
part2 xs = snd . maximum . map (length &&& strength) $ bridges xs 0

main :: IO ()
main = do
  pieces <- getComponents
  print pieces
  print . part1 $ pieces
  print . part2 $ pieces
