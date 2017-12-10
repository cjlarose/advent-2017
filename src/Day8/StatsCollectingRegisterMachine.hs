module Day8.StatsCollectingRegisterMachine (interpret) where

import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Monad.State (runState, get)
import Control.Monad.Trans (lift)
import Data.Semigroup (Max(..))
import Control.Monad (forM_)
import Day8.AST (ConditionalStatement)
import Day8.RegisterState (greatestValue)
import Day8.RegisterMachine (RegisterMachine, evaluate)

evaluateStmt :: ConditionalStatement -> WriterT (Max Int) RegisterMachine ()
evaluateStmt stmt = do
  lift $ evaluate stmt
  snapshot <- lift get
  tell . Max . greatestValue $ snapshot

interpretWithLog :: [ConditionalStatement] -> RegisterMachine (Max Int)
interpretWithLog xs = execWriterT $ forM_ xs evaluateStmt

interpret :: [ConditionalStatement] -> (Int, Int)
interpret xs = (greatestValue regs, greatestEver)
  where (Max greatestEver, regs) = runState (interpretWithLog xs) mempty
