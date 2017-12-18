module Advent2017.Input (getInputAsString, getFirstLineOfInput) where

getInputAsString :: String -> IO String
getInputAsString name = readFile $ "inputs/" ++ name ++ ".txt"

getFirstLineOfInput :: String -> IO String
getFirstLineOfInput name = head . lines <$> getInputAsString name
