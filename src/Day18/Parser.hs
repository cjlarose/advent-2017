module Day18.Parser (instructions) where

import Data.Attoparsec.Text (Parser, endOfLine, endOfInput, char, string, sepBy, decimal, anyChar, choice, signed)
import Data.Text (pack)
import Day18.AST (Register, ReferenceOrImmediate(..), Instruction(..))

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
