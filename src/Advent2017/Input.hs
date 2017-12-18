module Advent2017.Input (getInputAsString, getInputAsText, getFirstLineOfInput) where

import Data.Text (Text)
import qualified Data.Text.IO

path :: String -> String
path name = "inputs/" ++ name ++ ".txt"

getInputAsString :: String -> IO String
getInputAsString = readFile . path 

getInputAsText :: String -> IO Text
getInputAsText = Data.Text.IO.readFile . path

getFirstLineOfInput :: String -> IO String
getFirstLineOfInput name = head . lines <$> getInputAsString name
