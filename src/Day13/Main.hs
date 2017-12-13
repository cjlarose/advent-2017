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

smallestPossibleSafeDelay :: Firewall -> PicoTime
smallestPossibleSafeDelay fw =
  fst . head . filter ((==0) . snd) . map (\t -> (t, sev t)) $ [0 ..]
 where
  sev :: PicoTime -> Int
  sev t0 =
    sum
      . map (\(d, r) -> if position r (d + t0) == 0 then 1 else 0)
      . Map.assocs
      $ fw

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly firewall . pack $ input
  case parsed of
    Left  err -> print err
    Right ast -> do
      let fw = Map.fromList ast
      print . tripSeverity $ fw
      print . smallestPossibleSafeDelay $ fw
