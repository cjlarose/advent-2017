module Day23.Main (main) where

import Data.Attoparsec.Text (Parser, parseOnly, sepBy, endOfLine, string, space, decimal, signed, choice, letter, endOfInput)
import Data.Text (pack)
import Control.Applicative (liftA2)
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed ((!), (//))
import Advent2017.Input (getInputAsText)

type Register = Char
data ReferenceOrImmediate = Reference Register | Immediate Int deriving (Show)
data Instruction = SET Register ReferenceOrImmediate
                 | SUB Register ReferenceOrImmediate
                 | MUL Register ReferenceOrImmediate
                 | JNZ ReferenceOrImmediate ReferenceOrImmediate deriving (Show)
type Program = [Instruction]
type RegisterState = UArray.Array Char Int
type Machine = (Int, RegisterState)

register :: Parser Register
register = letter

referenceOrImmediate :: Parser ReferenceOrImmediate
referenceOrImmediate =
  choice [Reference <$> register, Immediate <$> signed decimal]

registerModifyingInstruction
  :: (Register -> ReferenceOrImmediate -> Instruction)
  -> String
  -> Parser Instruction
registerModifyingInstruction f mnemonic = liftA2
  f
  (string (pack mnemonic) *> space *> register)
  (space *> referenceOrImmediate)

jumpInstruction :: Parser Instruction
jumpInstruction = liftA2
  JNZ
  (string (pack "jnz") *> space *> referenceOrImmediate)
  (space *> referenceOrImmediate)

instruction :: Parser Instruction
instruction = choice
  [ registerModifyingInstruction SET "set"
  , registerModifyingInstruction SUB "sub"
  , registerModifyingInstruction MUL "mul"
  , jumpInstruction
  ]

program :: Parser Program
program = instruction `sepBy` endOfLine <* endOfLine <* endOfInput

newMachine :: Machine
newMachine = (0, regs)
  where regs = UArray.array ('a', 'h') $ map (\r -> (r, 0)) ['a' .. 'h']

valueOf :: ReferenceOrImmediate -> Machine -> Int
valueOf (Immediate x) _         = x
valueOf (Reference k) (_, regs) = regs ! k

runInstruction :: Machine -> Instruction -> Machine
runInstruction m@(pc, regs) (SET x y) = (pc + 1, regs // [(x, valueOf y m)])
runInstruction m@(pc, regs) (SUB x y) =
  (pc + 1, regs // [(x, regs ! x - valueOf y m)])
runInstruction m@(pc, regs) (MUL x y) =
  (pc + 1, regs // [(x, regs ! x * valueOf y m)])
runInstruction m@(pc, regs) (JNZ x y) =
  let k = if valueOf x m /= 0 then valueOf y m else 1 in (pc + k, regs)

runMachine :: Machine -> Program -> (Machine, Int)
runMachine machine prog = go machine 0
 where
  go :: Machine -> Int -> (Machine, Int)
  go m@(pc, _) acc
    | pc < 0 || pc >= length prog
    = (m, acc)
    | otherwise
    = let inst = prog !! pc in go (runInstruction m inst) (newAcc inst)
   where
    newAcc (MUL _ _) = acc + 1
    newAcc _         = acc

getProgram :: IO Program
getProgram = either (const []) id . parseOnly program <$> getInputAsText "23"

part1 :: Program -> Int
part1 = snd . runMachine newMachine

part2 :: Program -> Int
part2 prog = finalRegs ! 'h'
 where
  (_, regs) = newMachine
  initialState = (0, regs // [('a', 1)])
  (_, finalRegs) = fst $ runMachine initialState prog

main :: IO ()
main = do
  prog <- getProgram
  print . part1 $ prog
  print . part2 $ prog
