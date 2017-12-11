module Day8.Main (solve) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (pack)
import Day8.Parser (program)
import Day8.StatsCollectingRegisterMachine (interpret)

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly program (pack input)
  case parsed of
    Left  _   -> print "err"
    Right ast -> print $ interpret ast
