module Day18.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Map.Strict as Map
import Day18.AST (Instruction(..))
import Day18.Parser (instructions)
import Day18.Machine (Machine, readVal, deref)

advance :: Machine -> [Instruction] -> Either Int Machine
advance m@(pc, lastSound, regs) xs = go (xs !! pc)
 where
  go (SND k    ) = Right (pc + 1, readVal k m, regs)
  go (SET k ref) = Right (pc + 1, lastSound, Map.insert k (deref ref m) regs)
  go (ADD k ref) =
    Right (pc + 1, lastSound, Map.insert k (readVal k m + deref ref m) regs)
  go (MUL k ref) =
    Right (pc + 1, lastSound, Map.insert k (readVal k m * deref ref m) regs)
  go (MOD k ref) =
    Right (pc + 1, lastSound, Map.insert k (readVal k m `mod` deref ref m) regs)
  go (RCV k) =
    if readVal k m == 0 then Right (pc + 1, lastSound, regs) else Left lastSound
  go (JGZ k ref) =
    let newPc = (if readVal k m > 0 then pc + deref ref m else pc + 1)
    in  Right (newPc, lastSound, regs)

runMachineUntilRecovery :: Machine -> [Instruction] -> Int
runMachineUntilRecovery m xs = case advance m xs of
  Left  lastSound  -> lastSound
  Right newMachine -> runMachineUntilRecovery newMachine xs

part1 :: [Instruction] -> Int
part1 = runMachineUntilRecovery (0, -1, Map.empty)

main :: IO ()
main = do
  input <- getInputAsText "18"
  let parsed = parseOnly instructions input
  case parsed of
    Left  err -> print err
    Right ast -> print . part1 $ ast
