module Day8.Main (solve) where

import qualified Data.Map.Strict as Map
import Data.List (maximum)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.UTF8 (fromString)
import Day8.Parser (instructionList)
import Day8.Interpreter (interpret)

largestValue :: Map.Map String Int -> Int
largestValue regs = max (maximum . Map.elems $ regs) 0

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly instructionList (fromString input)
  case parsed of
    Left  _   -> print "err"
    Right ast -> do
      let result = interpret ast
      print result
      print . largestValue $ result
