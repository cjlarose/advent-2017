module Day18.Machine (Machine, newMachine, readVal, setVal, deref, advancePc, jumpRelative, readPc) where

import qualified Data.Map.Strict as Map
import Day18.AST (Register, ReferenceOrImmediate(..))

type Machine = (Int, Map.Map Register Int)

newMachine :: Machine
newMachine = (0, Map.empty)

readVal :: Char -> Machine -> Int
readVal k (_, regs) = Map.findWithDefault 0 k regs

readPc :: Machine -> Int
readPc (x, _) = x

setVal :: Char -> Int -> Machine -> Machine
setVal k v (pc, regs) = (pc, Map.insert k v regs)

advancePc :: Machine -> Machine
advancePc = jumpRelative 1

jumpRelative :: Int -> Machine -> Machine
jumpRelative k (pc, regs) = (pc + k, regs)

deref :: ReferenceOrImmediate -> Machine -> Int
deref (Reference k) m = readVal k m
deref (Immediate x) _ = x
