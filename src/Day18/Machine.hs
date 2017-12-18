module Day18.Machine (Machine, newMachine, readVal, setVal, setSound, deref, advancePc, jumpRelative, readLastSound, readPc) where

import qualified Data.Map.Strict as Map
import Day18.AST (Register, ReferenceOrImmediate(..))

type Machine = (Int, Int, Map.Map Register Int)

newMachine :: Machine
newMachine = (0, -1, Map.empty)

readVal :: Char -> Machine -> Int
readVal k (_, _, regs) = Map.findWithDefault 0 k regs

readLastSound :: Machine -> Int
readLastSound (_, x, _) = x

readPc :: Machine -> Int
readPc (x, _, _) = x

setVal :: Char -> Int -> Machine -> Machine
setVal k v (pc, sound, regs) = (pc, sound, Map.insert k v regs)

setSound :: Int -> Machine -> Machine
setSound v (pc, _, regs) = (pc, v, regs)

advancePc :: Machine -> Machine
advancePc = jumpRelative 1

jumpRelative :: Int -> Machine -> Machine
jumpRelative k (pc, sound, regs) = (pc + k, sound, regs)

deref :: ReferenceOrImmediate -> Machine -> Int
deref (Reference k) m = readVal k m
deref (Immediate x) _ = x
