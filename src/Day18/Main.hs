module Day18.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (parseOnly)
import Day18.AST (Instruction(..))
import Day18.Parser (instructions)
import Day18.Machine (Machine, newMachine, readVal, readLastSound, setVal, deref, setSound, advancePc, jumpRelative, readPc)

advance :: Machine -> [Instruction] -> Either Int Machine
advance m xs = go (xs !! readPc m)
 where
  go (SND k    ) = Right . advancePc . setSound (readVal k m) $ m
  go (SET k ref) = Right . advancePc . setVal k (deref ref m) $ m
  go (ADD k ref) = Right . advancePc . setVal k (readVal k m + deref ref m) $ m
  go (MUL k ref) = Right . advancePc . setVal k (readVal k m * deref ref m) $ m
  go (MOD k ref) =
    Right . advancePc . setVal k (readVal k m `mod` deref ref m) $ m
  go (RCV k) =
    if readVal k m == 0 then Right (advancePc m) else Left (readLastSound m)
  go (JGZ k ref) = Right
    $ if readVal k m > 0 then jumpRelative (deref ref m) m else advancePc m

runMachineUntilRecovery :: Machine -> [Instruction] -> Int
runMachineUntilRecovery m xs = case advance m xs of
  Left  lastSound -> lastSound
  Right m'        -> runMachineUntilRecovery m' xs

part1 :: [Instruction] -> Int
part1 = runMachineUntilRecovery newMachine

main :: IO ()
main = do
  input <- getInputAsText "18"
  let parsed = parseOnly instructions input
  case parsed of
    Left  err -> print err
    Right ast -> print . part1 $ ast
