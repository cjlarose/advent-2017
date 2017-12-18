module Day18.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (Parser, parseOnly, endOfLine, endOfInput, char, string, sepBy, decimal, anyChar, choice, signed)
import qualified Data.Map.Strict as Map
import Data.Text (pack)

type Register = Char
data ReferenceOrImmediate = Reference Register | Immediate Int deriving (Show)
data Instruction = Sound Register
                 | Set Register ReferenceOrImmediate
                 | Add Register ReferenceOrImmediate
                 | Multiply Register ReferenceOrImmediate
                 | Mod Register ReferenceOrImmediate
                 | RecoverNZ Register
                 | JumpGZ Register ReferenceOrImmediate deriving (Show)

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
  [ singleArgumentInstruction "snd" Sound
  , doubleArgumentInstruction "set" Set
  , doubleArgumentInstruction "add" Add
  , doubleArgumentInstruction "mul" Multiply
  , doubleArgumentInstruction "mod" Mod
  , singleArgumentInstruction "rcv" RecoverNZ
  , doubleArgumentInstruction "jgz" JumpGZ
  ]

instructions :: Parser [Instruction]
instructions = (instruction `sepBy` endOfLine) <* endOfLine <* endOfInput

getRegister :: Char -> Machine -> Int
getRegister k (_, _, regs) = Map.findWithDefault 0 k regs

runMachineUntilRecover :: Machine -> [Instruction] -> Machine
runMachineUntilRecover (pc, lastSound, xs) _ = (pc, lastSound, xs)

main :: IO ()
main = do
  input <- getInputAsText "18"
  let parsed = parseOnly instructions input
  case parsed of
    Left  err -> print err
    Right ast -> print ast
