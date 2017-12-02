module Day1.Main where

import Data.Char (digitToInt, isDigit)

captcha :: Int -> [Int] -> Int
captcha n xs =
  sum . map fst . filter (uncurry (==)) . zip xs . drop n $ cycle xs

solve :: String -> IO ()
solve input = do
  let digits = map digitToInt . filter isDigit $ input
  print $ captcha 1 digits
  print $ captcha (length digits `div` 2) digits
