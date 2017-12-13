module Day13.Main (solve) where

import qualified Data.Map.Strict as Map
import Data.Attoparsec.Text (Parser, parseOnly, sepBy, string, endOfLine, endOfInput, decimal)
import Control.Applicative (liftA2)
import Data.Text (pack)

type Depth = Int
type Range = Int
type PicoTime = Int
type Firewall = Map.Map Depth Range

firewall :: Parser [(Depth, Range)]
firewall = (layer `sepBy` endOfLine) <* endOfLine <* endOfInput
 where
  layer :: Parser (Depth, Range)
  layer = liftA2 (,) (decimal <* string (pack ": ")) decimal

position :: Range -> PicoTime -> Int
position r t = let peak = r - 1 in peak - abs (peak - (t `mod` (2 * peak)))

tripSeverity :: Firewall -> Int
tripSeverity =
  sum . map (\(d, r) -> if position r d == 0 then d * r else 0) . Map.assocs

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly firewall . pack $ input
  case parsed of
    Left  err -> print err
    Right ast -> print . tripSeverity . Map.fromList $ ast
