module Day3.Main where

distance :: Int -> Int
distance x = abs (((x - (n * n) - 1) `mod` (n + 1)) - dv) + n - dv
 where
  odds = [ z * 2 + 1 | z <- [0 ..] ]
  n    = (head . filter (\y -> (y * y) > x) $ odds) - 2
  dv   = (n - 1) `div` 2

solve :: String -> IO ()
solve input = do
  let x = read input :: Int
  print . distance $ x
