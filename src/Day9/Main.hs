module Day9.Main where

import Advent2017.Input (getInputAsText)
import Data.Attoparsec.Text (Parser, endOfLine, parseOnly, char, endOfInput, sepBy, choice, anyChar, notChar, many')
import Data.Maybe (catMaybes)

data GroupMember = Garbage String | Group [GroupMember] deriving (Show)

garbageString :: Parser String
garbageString = catMaybes <$> many' garbageChar
 where
  garbageChar = choice [Nothing <$ (char '!' *> anyChar), Just <$> notChar '>']

garbage :: Parser GroupMember
garbage = char '<' *> (Garbage <$> garbageString) <* char '>'

groupMember :: Parser GroupMember
groupMember = choice [group, garbage]

group :: Parser GroupMember
group = char '{' *> (Group <$> (groupMember `sepBy` char ',') <* char '}')

score' :: Int -> GroupMember -> Int
score' _ (Garbage _ ) = 0
score' p (Group   xs) = let n = 1 + p in sum $ n : map (score' n) xs

score :: GroupMember -> Int
score = score' 0

extractGarbage :: GroupMember -> String
extractGarbage (Garbage s ) = s
extractGarbage (Group   xs) = concatMap extractGarbage xs

main :: IO ()
main = do
  input <- getInputAsText "9"
  let parsed = parseOnly (group <* endOfLine <* endOfInput) input
  case parsed of
    Left  _   -> print "err"
    Right ast -> do
      print . score $ ast
      print . length . extractGarbage $ ast
