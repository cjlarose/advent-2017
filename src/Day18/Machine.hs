module Day18.Machine (Machine, readVal, deref) where

import qualified Data.Map.Strict as Map
import Day18.AST (Register, ReferenceOrImmediate(..))

type Machine = (Int, Int, Map.Map Register Int)

readVal :: Char -> Machine -> Int
readVal k (_, _, regs) = Map.findWithDefault 0 k regs

deref :: ReferenceOrImmediate -> Machine -> Int
deref (Reference k) m = readVal k m
deref (Immediate x) _ = x
