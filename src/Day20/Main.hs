module Day20.Main (main) where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (Parser, parseOnly, endOfLine, endOfInput, char, string, sepBy, signed, decimal)
import Data.Text (pack)
import Data.List (minimumBy, sortOn, groupBy)
import Data.Ord (comparing)

type Vec3 = (Int, Int, Int)
data Particle = Particle { position :: Vec3
                         , velocity :: Vec3
                         , acceleration :: Vec3 } deriving (Show)

vec3 :: Parser Vec3
vec3 =
  (,,)
    <$> signed decimal
    <*> (char ',' *> signed decimal)
    <*> (char ',' *> signed decimal)

particle :: Parser Particle
particle =
  Particle
    <$> assignment 'p'
    <*> (string (pack ", ") *> assignment 'v')
    <*> (string (pack ", ") *> assignment 'a')
 where
  inBrackets p = char '<' *> p <* char '>'
  assignment c = char c *> char '=' *> inBrackets vec3

particles :: Parser [Particle]
particles = particle `sepBy` endOfLine <* endOfLine <* endOfInput

addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (i, j, k) (i', j', k') = (i + i', j + j', k + k')

moveParticle :: Particle -> Particle
moveParticle p = p { position = newP, velocity = newV }
 where
  newV = velocity p `addVec3` acceleration p
  newP = position p `addVec3` newV

closestToOrigin :: [Particle] -> Int
closestToOrigin = fst . minimumBy (comparing (d . snd)) . zip [0 ..]
  where d p = let (x, y, z) = position p in abs x + abs y + abs z

part1 :: [Particle] -> Int
part1 xs = closestToOrigin $ iterate (map moveParticle) xs !! 1000

resolveCollisions :: [Particle] -> [Particle]
resolveCollisions =
  concat
    . filter ((==1) . length)
    . groupBy (\p p' -> position p == position p')
    . sortOn position

part2 :: [Particle] -> Int
part2 xs = length $ iterate (resolveCollisions . map moveParticle) xs !! 1000

main :: IO ()
main = do
  parsed <- parseOnly particles <$> getInputAsText "20"
  case parsed of
    Left  err -> print err
    Right ast -> do
      print . part1 $ ast
      print . part2 $ ast
