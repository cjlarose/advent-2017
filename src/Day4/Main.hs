module Day4.Main where

import Data.List (nub)

isValidPassphrase :: String -> Bool
isValidPassphrase phrase = length xs == length (nub xs)
  where xs = words phrase

solve :: String -> IO ()
solve input = do
  let passphrases = lines input
  print . length . filter isValidPassphrase $ passphrases
