module Day8.RegisterState (RegisterState, binding, registerValue, greatestValue) where

import qualified Data.Map.Strict as Map
import Day8.AST (RegisterName)

newtype RegisterState = RegisterState (Map.Map RegisterName Int)

instance Monoid RegisterState where
  mempty = RegisterState Map.empty
  mappend (RegisterState old) (RegisterState new) = RegisterState $ Map.union new old

binding :: RegisterName -> Int -> RegisterState
binding reg v = RegisterState $ Map.singleton reg v

registerValue :: RegisterName -> RegisterState -> Int
registerValue reg (RegisterState m) = Map.findWithDefault 0 reg m

greatestValue :: RegisterState -> Int
greatestValue (RegisterState regs)
  | Map.null regs = 0
  | otherwise     = max (maximum . Map.elems $ regs) 0
