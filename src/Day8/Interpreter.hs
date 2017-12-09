module Day8.Interpreter (interpret) where

import qualified Data.Map.Strict as Map
import Control.Monad.State (State, execState, get, put)
import Control.Monad (when, mapM)
import Day8.AST (ConditionalStatement(..), RegisterName, BooleanExpression(..), BinOp(..), IncrementStatement(..))

type RegisterState = Map.Map RegisterName Int

lookupRegister :: RegisterName -> State RegisterState Int
lookupRegister reg = Map.findWithDefault 0 reg <$> get

getBinOp :: BinOp -> (Int -> Int -> Bool)
getBinOp Greater     = (>)
getBinOp Less        = (<)
getBinOp GreaterOrEq = (>=)
getBinOp LessOrEq    = (<=)
getBinOp Equal       = (==)
getBinOp NotEqual    = (/=)

evaluateBoolExpr :: BooleanExpression -> State RegisterState Bool
evaluateBoolExpr (BooleanExpression reg op int) = flip (getBinOp op) int <$> lookupRegister reg

evaluateIncrStmt :: IncrementStatement -> State RegisterState ()
evaluateIncrStmt (IncrementStatement reg int) = do
  val <- lookupRegister reg
  let newVal = val + int
  regs <- get
  put $ Map.insert reg newVal regs

evaluate :: ConditionalStatement -> State RegisterState ()
evaluate (ConditionalStatement b stmt) = do
  proceed <- evaluateBoolExpr b
  when proceed $ evaluateIncrStmt stmt

interpret :: [ConditionalStatement] -> RegisterState
interpret xs = execState (mapM evaluate xs) Map.empty
