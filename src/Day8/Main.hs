module Day8.Main where

import Advent2017.Input (getInputAsString)
import Data.Attoparsec.Text (parseOnly)
import Data.Text (pack)
import Day8.Parser (program)
import Day8.StatsCollectingRegisterMachine (interpret)

main :: IO ()
main = do
  input <- getInputAsString "8"
  let parsed = parseOnly program (pack input)
  case parsed of
    Left  _   -> print "err"
    Right ast -> print $ interpret ast
