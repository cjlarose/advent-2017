module Day8.RegisterMachine (RegisterMachine, evaluate) where

import Control.Monad.State (State, get, modify)
import Control.Monad (when)
import Day8.RegisterState (RegisterState, binding, registerValue)
import Day8.AST (ConditionalStatement(..), RegisterName, BooleanExpression(..), BinOp(..), IncrementStatement(..))

type RegisterMachine = State RegisterState

lookupRegister :: RegisterName -> RegisterMachine Int
lookupRegister reg = registerValue reg <$> get

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
