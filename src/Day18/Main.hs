module Day18.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (Parser, parseOnly, endOfLine, endOfInput, char, string, sepBy, decimal, anyChar, choice, signed)
import qualified Data.Map.Strict as Map
import Data.Text (pack)

type Register = Char
data ReferenceOrImmediate = Reference Register | Immediate Int deriving (Show)
data Instruction = SND Register
                 | SET Register ReferenceOrImmediate
                 | ADD Register ReferenceOrImmediate
                 | MUL Register ReferenceOrImmediate
                 | MOD Register ReferenceOrImmediate
                 | RCV Register
                 | JGZ Register ReferenceOrImmediate deriving (Show)

type Machine = (Int, Int, Map.Map Register Int)

singleArgumentInstruction
  :: String -> (Register -> Instruction) -> Parser Instruction
singleArgumentInstruction mnemonic f =
  f <$> (string (pack mnemonic) *> char ' ' *> anyChar)

doubleArgumentInstruction
  :: String
  -> (Register -> ReferenceOrImmediate -> Instruction)
  -> Parser Instruction
doubleArgumentInstruction mnemonic f =
  f
    <$> (string (pack mnemonic) *> char ' ' *> anyChar)
    <*> (  char ' '
        *> choice [Immediate <$> signed decimal, Reference <$> anyChar]
        )

instruction :: Parser Instruction
instruction = choice
  [ singleArgumentInstruction "snd" SND
  , doubleArgumentInstruction "set" SET
  , doubleArgumentInstruction "add" ADD
  , doubleArgumentInstruction "mul" MUL
  , doubleArgumentInstruction "mod" MOD
  , singleArgumentInstruction "rcv" RCV
  , doubleArgumentInstruction "jgz" JGZ
  ]

instructions :: Parser [Instruction]
instructions = (instruction `sepBy` endOfLine) <* endOfLine <* endOfInput

readVal :: Char -> Machine -> Int
readVal k (_, _, regs) = Map.findWithDefault 0 k regs

deref :: ReferenceOrImmediate -> Machine -> Int
deref (Reference k) m = readVal k m
deref (Immediate x) _ = x

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
