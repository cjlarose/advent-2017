module Day8.Main (solve) where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.UTF8 (fromString)
import Day8.Parser (program)
import Day8.StatsCollectingRegisterMachine (interpret)

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly program (fromString input)
  case parsed of
    Left  _   -> print "err"
    Right ast -> print $ interpret ast
