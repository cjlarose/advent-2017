module Day18.Main (main) where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (parseOnly)
import Day18.AST (Instruction(..))
import Day18.Parser (instructions)
import Day18.Machine (Machine, newMachine, readVal, setVal, deref, advancePc, jumpRelative, readPc)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>), ViewL((:<)))

data PausedMachine = Receiving (Int -> PausedMachine) | Sending (Int, Machine)

runUntilCommunicating :: Machine -> [Instruction] -> PausedMachine
runUntilCommunicating m xs = go (xs !! readPc m)
 where
  next :: Machine -> PausedMachine
  next m' = runUntilCommunicating m' xs

  go :: Instruction -> PausedMachine
  go (SND ref  ) = Sending (deref ref m, advancePc m)
  go (SET k ref) = next . advancePc . setVal k (deref ref m) $ m
  go (ADD k ref) = next . advancePc . setVal k (readVal k m + deref ref m) $ m
  go (MUL k ref) = next . advancePc . setVal k (readVal k m * deref ref m) $ m
  go (MOD k ref) =
    next . advancePc . setVal k (readVal k m `mod` deref ref m) $ m
  go (RCV k) =
    Receiving (\received -> next . advancePc . setVal k received $ m)
  go (JGZ a b) =
    next $ (if deref a m > 0 then jumpRelative (deref b m) else advancePc) m

part1 :: [Instruction] -> Int
part1 xs = last . go $ runUntilCommunicating (newMachine 0) xs
 where
  go :: PausedMachine -> [Int]
  go (Sending (v, m)) = v : go (runUntilCommunicating m xs)
  go (Receiving _) = []

part2 :: [Instruction] -> Int
part2 xs = go (runUntilCommunicating (newMachine 0) xs, Seq.empty)
              (runUntilCommunicating (newMachine 1) xs, Seq.empty)
              0
 where
  run :: Machine -> PausedMachine
  run m = runUntilCommunicating m xs

  go
    :: (PausedMachine, Seq.Seq Int)
    -> (PausedMachine, Seq.Seq Int)
    -> Int
    -> Int
  go (Sending (v0, m0), inbox0) (m1, inbox1) acc =
    go (run m0, inbox0) (m1, inbox1 |> v0) acc
  go (m0, inbox0) (Sending (v1, m1), inbox1) acc =
    go (m0, inbox0 |> v1) (run m1, inbox1) (acc + 1)
  go (Receiving f, inbox0) (Receiving g, inbox1) acc
    | Seq.EmptyL <- Seq.viewl inbox0, Seq.EmptyL <- Seq.viewl inbox1 = acc
    | v0:<rest0 <- Seq.viewl inbox0 = go (f v0, rest0) (Receiving g, inbox1) acc
    | v1:<rest1 <- Seq.viewl inbox1 = go (Receiving f, inbox0) (g v1, rest1) acc
    | otherwise                     = error "unreachable"

main :: IO ()
main = do
  input <- getInputAsText "18"
  let parsed = parseOnly instructions input
  case parsed of
    Left  err -> print err
    Right ast -> do
      print . part1 $ ast
      print . part2 $ ast
