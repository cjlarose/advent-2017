module Day8.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (parseOnly)
import Day8.Parser (program)
import Day8.StatsCollectingRegisterMachine (interpret)

main :: IO ()
main = do
  parsed <- parseOnly program <$> getInputAsText "8"
  case parsed of
    Left  _   -> print "err"
    Right ast -> print $ interpret ast
