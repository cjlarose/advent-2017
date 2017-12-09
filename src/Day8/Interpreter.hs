module Day8.Interpreter (interpret, RegisterState(..)) where

import qualified Data.Map.Strict as Map
import Control.Monad.State (State, execState, get, modify)
import Control.Monad (when, mapM)
import Day8.AST (ConditionalStatement(..), RegisterName, BooleanExpression(..), BinOp(..), IncrementStatement(..))

newtype RegisterState = RegisterState (Map.Map RegisterName Int)

instance Monoid RegisterState where
  mempty = RegisterState Map.empty
  mappend (RegisterState old) (RegisterState new) = RegisterState $ Map.union new old

binding :: RegisterName -> Int -> RegisterState
binding reg v = RegisterState $ Map.singleton reg v

lookupRegister :: RegisterName -> State RegisterState Int
lookupRegister reg = (\(RegisterState m) -> Map.findWithDefault 0 reg m) <$> get

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
evaluateIncrStmt (IncrementStatement reg int) =
  lookupRegister reg >>= (\val -> modify (`mappend`binding reg (val + int)))

evaluate :: ConditionalStatement -> State RegisterState ()
evaluate (ConditionalStatement b stmt) = do
  proceed <- evaluateBoolExpr b
  when proceed $ evaluateIncrStmt stmt

interpret :: [ConditionalStatement] -> RegisterState
interpret xs = execState (mapM evaluate xs) mempty
