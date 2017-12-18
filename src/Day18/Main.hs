module Day18.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (parseOnly)
import Day18.AST (Instruction(..))
import Day18.Parser (instructions)
import Day18.Machine (Machine, newMachine, readVal, setVal, deref, advancePc, jumpRelative, readPc)
import Debug.Trace (traceShow)

listen :: Machine -> [Instruction] -> [Int]
listen m xs = go (xs !! readPc m)
 where
  next m' = listen m' xs

  go (SND ref  ) = deref ref m : next (advancePc m)
  go (SET k ref) = next . advancePc . setVal k (deref ref m) $ m
  go (ADD k ref) = next . advancePc . setVal k (readVal k m + deref ref m) $ m
  go (MUL k ref) = next . advancePc . setVal k (readVal k m * deref ref m) $ m
  go (MOD k ref) =
    next . advancePc . setVal k (readVal k m `mod` deref ref m) $ m
  go (RCV k) = if readVal k m == 0 then next . advancePc $ m else []
  go (JGZ a b) =
    next $ (if deref a m > 0 then jumpRelative (deref b m) else advancePc) m

sentValues :: Int -> Machine -> [Instruction] -> [Int] -> [Int]
sentValues progId m xs inbox = go (xs !! readPc m)
 where
  next m' = sentValues progId m' xs inbox

  go (SND ref) =
    traceShow (show progId ++ ": sending " ++ show (deref ref m)) deref ref m
      : next (advancePc m)
  go (SET k ref) = next . advancePc . setVal k (deref ref m) $ m
  go (ADD k ref) = next . advancePc . setVal k (readVal k m + deref ref m) $ m
  go (MUL k ref) = next . advancePc . setVal k (readVal k m * deref ref m) $ m
  go (MOD k ref) =
    next . advancePc . setVal k (readVal k m `mod` deref ref m) $ m
  go (RCV k) =
    let (received:rest) = inbox
    in  traceShow (show progId ++ ": received " ++ show received)
                  (sentValues progId (advancePc . setVal k received $ m) xs rest)
  go (JGZ a b) =
    next $ (if deref a m > 0 then jumpRelative (deref b m) else advancePc) m

part1 :: [Instruction] -> Int
part1 xs = last $ listen (newMachine 0) xs

part2 :: [Instruction] -> [Int]
part2 xs =
  let m0 = sentValues 0 (newMachine 0) xs m1
      m1 = sentValues 1 (newMachine 1) xs m0
  in  m0

main :: IO ()
main = do
  input <- getInputAsText "18"
  let parsed = parseOnly instructions input
  case parsed of
    Left  err -> print err
    Right ast -> do
      print . part1 $ ast
      print . length . part2 $ ast
