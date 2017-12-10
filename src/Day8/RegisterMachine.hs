module Day8.RegisterMachine (RegisterMachine, RegisterState(..), evaluate) where

import qualified Data.Map.Strict as Map
import Control.Monad.State (State, get, modify)
import Control.Monad (when)
import Day8.AST (ConditionalStatement(..), RegisterName, BooleanExpression(..), BinOp(..), IncrementStatement(..))

newtype RegisterState = RegisterState (Map.Map RegisterName Int)

instance Monoid RegisterState where
  mempty = RegisterState Map.empty
  mappend (RegisterState old) (RegisterState new) = RegisterState $ Map.union new old

type RegisterMachine = State RegisterState

binding :: RegisterName -> Int -> RegisterState
binding reg v = RegisterState $ Map.singleton reg v

lookupRegister :: RegisterName -> RegisterMachine Int
lookupRegister reg =
  (\(RegisterState m) -> Map.findWithDefault 0 reg m) <$> get

getBinOp :: BinOp -> (Int -> Int -> Bool)
getBinOp Greater     = (>)
getBinOp Less        = (<)
getBinOp GreaterOrEq = (>=)
getBinOp LessOrEq    = (<=)
getBinOp Equal       = (==)
getBinOp NotEqual    = (/=)

evaluateBoolExpr :: BooleanExpression -> RegisterMachine Bool
evaluateBoolExpr (BooleanExpression reg op int) =
  flip (getBinOp op) int <$> lookupRegister reg

evaluateIncrStmt :: IncrementStatement -> RegisterMachine ()
evaluateIncrStmt (IncrementStatement reg int) =
  lookupRegister reg >>= (\val -> modify (`mappend`binding reg (val + int)))

evaluate :: ConditionalStatement -> RegisterMachine ()
evaluate (ConditionalStatement b stmt) = do
  proceed <- evaluateBoolExpr b
  when proceed $ evaluateIncrStmt stmt
