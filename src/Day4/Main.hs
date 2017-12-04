module Day4.Main where

import Data.List (nubBy, sort)

isValidPassphrase :: (String -> String -> Bool) -> [String] -> Bool
isValidPassphrase wordEquality xs = xs == nubBy wordEquality xs

containsNoDuplicates :: [String] -> Bool
containsNoDuplicates = isValidPassphrase (==)

containsNoAnagrams :: [String] -> Bool
containsNoAnagrams = isValidPassphrase (\a b -> sort a == sort b)

solve :: String -> IO ()
solve input = do
  let passphrases = map words . lines $ input
  print . length . filter containsNoDuplicates $ passphrases
  print . length . filter containsNoAnagrams $ passphrases
