{-# LANGUAGE OverloadedStrings #-}

module Day25.Main (main) where

import Data.Attoparsec.Text (Parser, parseOnly, sepBy, endOfLine, decimal, endOfInput, char, inClass, satisfy, string, choice)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (find)
import Advent2017.Input (getInputAsText)

type State = Char
data Shift = L | R deriving Show
type TapeSymbol = Bool
type TransitionFunction = State -> TapeSymbol -> (State, TapeSymbol, Shift)
type Tape = Map.Map Int TapeSymbol
type TuringMachine = (State, TransitionFunction, Int, Tape)
type Blueprint = (State, Int, TransitionFunction)

stateName :: Parser State
stateName = satisfy . inClass $ "A-Z"

startState :: Parser State
startState = string "Begin in state " *> stateName <* char '.' <* endOfLine

checksumSpec :: Parser Int
checksumSpec =
  string "Perform a diagnostic checksum after "
    *> decimal
    <* string " steps."
    <* endOfLine

transitionFunction :: Parser TransitionFunction
transitionFunction = makeTransition <$> state `sepBy` endOfLine
 where
  decision :: Parser (State, TapeSymbol, Shift)
  decision =
    (\sym dir s -> (s, sym, dir))
      <$> (  string "    - Write the value "
          *> (toEnum <$> decimal)
          <* char '.'
          <* endOfLine
          )
      <*> (  string "    - Move one slot to the "
          *> choice [L <$ string "left", R <$ string "right"]
          <* char '.'
          <* endOfLine
          )
      <*> (  string "    - Continue with state "
          *> stateName
          <* char '.'
          <* endOfLine
          )

  tapeDecision :: Char -> Parser (State, TapeSymbol, Shift)
  tapeDecision c =
    string "  If the current value is "
      *> char c
      *> string ":"
      *> endOfLine
      *> decision

  tapeDecisions :: Parser (TapeSymbol -> (State, TapeSymbol, Shift))
  tapeDecisions =
    (\x y z -> if z then y else x) <$> tapeDecision '0' <*> tapeDecision '1'

  state :: Parser (State, TapeSymbol -> (State, TapeSymbol, Shift))
  state =
    (,)
      <$> (string "In state " *> stateName)
      <*> (string ":" *> endOfLine *> tapeDecisions)

  makeTransition
    :: [(State, TapeSymbol -> (State, TapeSymbol, Shift))] -> TransitionFunction
  makeTransition xs x = snd . fromJust . find ((==x) . fst) $ xs

blueprint :: Parser Blueprint
blueprint =
  ((,,) <$> startState <*> (checksumSpec <* endOfLine) <*> transitionFunction)
    <* endOfInput

getBlueprint :: IO Blueprint
getBlueprint = do
  parsed <- parseOnly blueprint <$> getInputAsText "25"
  return . either error id $ parsed

moveHead :: Shift -> Int -> Int
moveHead L = pred
moveHead R = succ

stepMachine :: TuringMachine -> TuringMachine
stepMachine (s, delta, tapeHead, tape) = (newState, delta, newHead, newTape)
 where
  (newState, sym, dir) = delta s $ Map.findWithDefault False tapeHead tape
  newTape              = Map.insert tapeHead sym tape
  newHead              = moveHead dir tapeHead

runMachine :: Blueprint -> TuringMachine
runMachine (s, n, delta) = iterate stepMachine (s, delta, 0, Map.empty) !! n

checksum :: TuringMachine -> Int
checksum (_, _, _, tape) = length . filter (==True) . Map.elems $ tape

main :: IO ()
main = do
  spec <- getBlueprint
  print . checksum . runMachine $ spec
