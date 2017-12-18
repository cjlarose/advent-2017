module Day16.Main (solve) where

import Data.Attoparsec.Text (Parser, endOfLine, endOfInput, sepBy, decimal,
  letter, choice, char, parseOnly)
import Data.Text (pack)
import Data.List (foldl')
import Data.Array.Unboxed as UArray (UArray, (//), (!), array, bounds, assocs, elems)
import Data.Char (ord, chr)
import qualified Data.Map.Strict as Map

data DanceMove = Spin Int | Exchange Int Int | Partner Char Char deriving (Show)
type Dancers = UArray.UArray Int Char

danceMove :: Parser DanceMove
danceMove = choice
  [ Spin <$> (char 's' *> decimal)
  , Exchange <$> (char 'x' *> decimal) <*> (char '/' *> decimal)
  , Partner <$> (char 'p' *> letter) <*> (char '/' *> letter)
  ]

danceMoves :: Parser [DanceMove]
danceMoves = danceMove `sepBy` char ',' <* endOfLine <* endOfInput

move :: Dancers -> DanceMove -> Dancers
move xs m = xs // updates m
 where
  n :: Int
  n = succ . snd . bounds $ xs

  posOf :: Char -> Int
  posOf x = fst . head . filter ((==x) . snd) . assocs $ xs

  updates :: DanceMove -> [(Int, Char)]
  updates (Spin x) = map
    (\i -> (,) i $ if i < x then xs ! (n - x + i) else xs ! (i - x))
    [0 .. (n - 1)]
  updates (Exchange a b) = [(a, xs ! b), (b, xs ! a)]
  updates (Partner  x y) = [(posOf x, y), (posOf y, x)]

makeDancers :: Int -> Dancers
makeDancers n =
  UArray.array (0, n - 1) $ map (\x -> (x, chr (ord 'a' + x))) [0 .. (n - 1)] :: Dancers

dance :: Dancers -> [DanceMove] -> Dancers
dance = foldl' move

danceForAReallyLongTime :: Dancers -> [DanceMove] -> Dancers
danceForAReallyLongTime ds ms = go ds 1000000000 Map.empty
 where
  go :: Dancers -> Int -> Map.Map Dancers Dancers -> Dancers
  go xs 0 _ = xs
  go xs n cache
    | Map.member xs cache
    = let ys = cache Map.! xs in go ys (n - 1) cache
    | otherwise
    = let ys = dance xs ms in go ys (n - 1) $ Map.insert xs ys cache

solve :: String -> IO ()
solve input = do
  let parsed = parseOnly danceMoves . pack $ input
  case parsed of
    Left  err   -> print err
    Right moves -> do
      let dancers = makeDancers 16
      putStrLn . elems $ dance dancers moves
      putStrLn . elems $ danceForAReallyLongTime dancers moves
