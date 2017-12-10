module Day8.StatsCollectingRegisterMachine (interpret) where

import qualified Data.Map.Strict as Map
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Monad.State (runState, get)
import Control.Monad.Trans (lift)
import Data.Semigroup (Max(..))
import Control.Monad (forM_)
import Day8.AST (ConditionalStatement)
import Day8.RegisterMachine (RegisterMachine, RegisterState(..), evaluate)

largestValue :: Map.Map String Int -> Int
largestValue regs | Map.null regs = 0
                  | otherwise     = max (maximum . Map.elems $ regs) 0

evaluateStmt :: ConditionalStatement -> WriterT (Max Int) RegisterMachine ()
evaluateStmt stmt = do
  lift $ evaluate stmt
  (RegisterState snapshot) <- lift get
  tell . Max . largestValue $ snapshot

interpretWithLog :: [ConditionalStatement] -> RegisterMachine (Max Int)
interpretWithLog xs = execWriterT $ forM_ xs evaluateStmt

interpret :: [ConditionalStatement] -> (Int, Int)
interpret xs = (largestValue regs, greatestEver)
 where
  (Max greatestEver, RegisterState regs) =
    runState (interpretWithLog xs) mempty
