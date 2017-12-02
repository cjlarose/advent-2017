module Day1.Main where

import Data.Char (digitToInt, isDigit)

captcha :: Int -> [Int] -> Int
captcha n digits =
  sum
    . map fst
    . filter (uncurry (==))
    . take (length digits)
    . zip xs
    . drop n
    $ xs
  where xs = cycle digits

solve :: String -> IO ()
solve input = do
  let digits = map digitToInt . filter isDigit $ input
  print $ captcha 1 digits
  print $ captcha (length digits `div` 2) digits
