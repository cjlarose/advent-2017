module Advent2017.Input where

getInputAsString :: String -> IO String
getInputAsString name = readFile $ "inputs/" ++ name ++ ".txt"
