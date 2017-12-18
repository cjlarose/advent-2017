module Day13.Main where

import Advent2017.Input (getInputAsString)
import Data.List (find)
import Data.Attoparsec.Text (Parser, parseOnly, sepBy, string, endOfLine, endOfInput, decimal)
import Control.Applicative (liftA2)
import Data.Text (pack)
import Data.Maybe (fromJust)

type Depth = Int
type Range = Int
type PicoTime = Int
type Firewall = [(Depth,Range)]

firewall :: Parser Firewall
firewall = (layer `sepBy` endOfLine) <* endOfLine <* endOfInput
 where
  layer :: Parser (Depth, Range)
  layer = liftA2 (,) (decimal <* string (pack ": ")) decimal

position :: Range -> PicoTime -> Int
position r t = let peak = r - 1 in peak - abs (peak - (t `mod` (2 * peak)))

collisions :: PicoTime -> Firewall -> Firewall
collisions t0 = filter (\(d, r) -> position r (t0 + d) == 0)

tripSeverity :: Firewall -> Int
tripSeverity = sum . map (uncurry (*)) . collisions 0

smallestPossibleSafeDelay :: Firewall -> PicoTime
smallestPossibleSafeDelay fw =
  fromJust . find (\t -> null $ collisions t fw) $ [0 ..]

main :: IO ()
main = do
  input <- getInputAsString "13"
  let parsed = parseOnly firewall . pack $ input
  case parsed of
    Left  err -> print err
    Right fw  -> do
      print . tripSeverity $ fw
      print . smallestPossibleSafeDelay $ fw
