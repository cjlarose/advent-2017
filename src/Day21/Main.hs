module Day21.Main where

import Advent2017.Input (getInputAsText)
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed ((!))
import Data.Attoparsec.Text (Parser, parseOnly, endOfLine, endOfInput, char, string, sepBy, takeWhile1, inClass)
import Data.Text (pack, unpack)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Either (either)

type Image = UArray.Array (Int, Int) Bool

row :: Parser String
row = unpack <$> takeWhile1 (inClass ".#")

showImage :: Image -> String
showImage img = intercalate
  "\n"
  [ [ if img ! (i, j) then '#' else '.' | j <- [0 .. n'] ] | i <- [0 .. m'] ]
  where (_, (m', n')) = UArray.bounds img

makeImage :: [String] -> Image
makeImage rows = UArray.array ((0, 0), (n - 1, n - 1)) entries
 where
  n = length rows

  entries :: [((Int, Int), Bool)]
  entries = concat $ zipWith
    (\i xs -> zipWith (\j c -> ((i, j), c == '#')) [0 ..] xs)
    [0 ..]
    rows

image :: Parser Image
image = makeImage <$> row `sepBy` char '/'

enhancementRule :: Parser (Image, Image)
enhancementRule = (,) <$> (image <* string (pack " => ")) <*> image

enhancementRules :: Parser [(Image, Image)]
enhancementRules = enhancementRule `sepBy` endOfLine <* endOfLine <* endOfInput

transformImg :: ((Int, Int) -> (Int, Int) -> (Int, Int)) -> Image -> Image
transformImg f img = UArray.array
  ((0, 0), (m', n'))
  [ ((i, j), img ! f (m', n') (i, j)) | i <- [0 .. m'], j <- [0 .. n'] ]
  where (_, (m', n')) = UArray.bounds img

rotateRight :: Image -> Image
rotateRight = transformImg (\(m, _) (i, j) -> (m - j, i))

flipH :: Image -> Image
flipH = transformImg (\(_, n) (i, j) -> (i, n - j))

equivalenceClass :: Image -> [Image]
equivalenceClass img = [img, flipH img] >>= take 4 . iterate rotateRight

patternMap :: [(Image, Image)] -> Map.Map Image Image
patternMap =
  Map.fromList . concatMap (\(k, v) -> zip (equivalenceClass k) (repeat v))

crop :: (Int, Int) -> (Int, Int) -> Image -> Image
crop (i, j) (m, n) src = UArray.array
  ((0, 0), (m - 1, n - 1))
  [ ((i', j'), src ! (i' + i, j' + j))
  | i' <- [0 .. m - 1]
  , j' <- [0 .. n - 1]
  ]

splitImage :: Image -> [[Image]]
splitImage img =
  [ [ crop (i * splitSize, j * splitSize) (splitSize, splitSize) img
    | j <- [0 .. (m `div` splitSize) - 1]
    ]
  | i <- [0 .. (m `div` splitSize) - 1]
  ]
 where
  m'        = fst . snd . UArray.bounds $ img
  m         = m' + 1
  splitSize = if m `mod` 2 == 0 then 2 else 3

joinImages :: [[Image]] -> Image
joinImages xs = UArray.array
  ((0, 0), (m - 1, n - 1))
  [ ( (i, j)
    , xs
    !! (i `div` (m' + 1))
    !! (j `div` (n' + 1))
    !  (i `mod` (m' + 1), j `mod` (n' + 1))
    )
  | i <- [0 .. m - 1]
  , j <- [0 .. n - 1]
  ]
 where
  (m', n') = snd . UArray.bounds . head . head $ xs
  m        = (m' + 1) * length xs
  n        = (n' + 1) * length (head xs)

enhance :: Map.Map Image Image -> Image -> Image
enhance patterns = joinImages . map (map (\p -> patterns Map.! p)) . splitImage

startingImage :: Image
startingImage = makeImage [".#.", "..#", "###"]

countOnPixels :: Image -> Int
countOnPixels = length . filter (==True) . UArray.elems

getRules :: IO [(Image, Image)]
getRules = do
  parsed <- parseOnly enhancementRules <$> getInputAsText "21"
  return . either (const []) id $ parsed

doRounds :: Map.Map Image Image -> Int -> IO ()
doRounds rules n = do
  let finalImage = iterate (enhance rules) startingImage !! n
  print . countOnPixels $ finalImage

main :: IO ()
main = do
  rules <- patternMap <$> getRules
  doRounds rules 5
  doRounds rules 18
