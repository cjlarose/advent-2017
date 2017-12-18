module Day18.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (parseOnly)
import Day18.AST (Instruction(..))
import Day18.Parser (instructions)
import Day18.Machine (Machine, newMachine, readVal, setVal, deref, advancePc, jumpRelative, readPc)

listen :: Machine -> [Instruction] -> [Int]
listen m xs = go (xs !! readPc m)
 where
  next m' = listen m' xs

  go (SND k    ) = readVal k m : next (advancePc m)
  go (SET k ref) = next . advancePc . setVal k (deref ref m) $ m
  go (ADD k ref) = next . advancePc . setVal k (readVal k m + deref ref m) $ m
  go (MUL k ref) = next . advancePc . setVal k (readVal k m * deref ref m) $ m
  go (MOD k ref) =
    next . advancePc . setVal k (readVal k m `mod` deref ref m) $ m
  go (RCV k) = if readVal k m == 0 then next . advancePc $ m else []
  go (JGZ k ref) =
    next $ (if readVal k m > 0 then jumpRelative (deref ref m) else advancePc) m

part1 :: [Instruction] -> Int
part1 xs = last $ listen (newMachine 0) xs

main :: IO ()
main = do
  input <- getInputAsText "18"
  let parsed = parseOnly instructions input
  case parsed of
    Left  err -> print err
    Right ast -> print . part1 $ ast
