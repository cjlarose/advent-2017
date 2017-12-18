module Day18.Parser (instructions) where

import Data.Attoparsec.Text (Parser, endOfLine, endOfInput, char, string, sepBy, decimal, letter, choice, signed)
import Data.Text (pack)
import Day18.AST (Register, ReferenceOrImmediate(..), Instruction(..))

singleArgumentInstruction
  :: String -> (Register -> Instruction) -> Parser Instruction
singleArgumentInstruction mnemonic f =
  f <$> (string (pack mnemonic) *> char ' ' *> letter)

referenceOrImmediate :: Parser ReferenceOrImmediate
referenceOrImmediate =
  choice [Immediate <$> signed decimal, Reference <$> letter]

doubleArgumentInstruction
  :: String
  -> (Register -> ReferenceOrImmediate -> Instruction)
  -> Parser Instruction
doubleArgumentInstruction mnemonic f =
  f
    <$> (string (pack mnemonic) *> char ' ' *> letter)
    <*> (char ' ' *> referenceOrImmediate)

sndInstruction :: Parser Instruction
sndInstruction =
  SND <$> (string (pack "snd") *> char ' ' *> referenceOrImmediate)

jumpInstruction :: Parser Instruction
jumpInstruction =
  JGZ
    <$> (string (pack "jgz") *> char ' ' *> referenceOrImmediate)
    <*> (char ' ' *> referenceOrImmediate)

instruction :: Parser Instruction
instruction = choice
  [ sndInstruction
  , doubleArgumentInstruction "set" SET
  , doubleArgumentInstruction "add" ADD
  , doubleArgumentInstruction "mul" MUL
  , doubleArgumentInstruction "mod" MOD
  , singleArgumentInstruction "rcv" RCV
  , jumpInstruction
  ]

instructions :: Parser [Instruction]
instructions = (instruction `sepBy` endOfLine) <* endOfLine <* endOfInput
