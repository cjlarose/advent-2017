module Day2.Main where

checksum :: [[Int]] -> Int
checksum rows = sum $ zipWith (-) (map maximum rows) (map minimum rows)

solve :: String -> IO ()
solve input = do
  let rows = map (map read . words) . lines $ input :: [[Int]]
  print . checksum $ rows
