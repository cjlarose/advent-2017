module Day18.AST (Register, ReferenceOrImmediate(..), Instruction(..)) where

type Register = Char
data ReferenceOrImmediate = Reference Register | Immediate Int deriving (Show)
data Instruction = SND ReferenceOrImmediate
                 | SET Register ReferenceOrImmediate
                 | ADD Register ReferenceOrImmediate
                 | MUL Register ReferenceOrImmediate
                 | MOD Register ReferenceOrImmediate
                 | RCV Register
                 | JGZ ReferenceOrImmediate ReferenceOrImmediate deriving (Show)
