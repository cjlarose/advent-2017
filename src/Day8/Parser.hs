module Day8.Parser (instructionList) where

import Data.Attoparsec.ByteString (Parser, takeWhile1, inClass, choice, string, many', endOfInput)
import Data.Attoparsec.ByteString.Char8 (signed, decimal, space, endOfLine)
import Data.ByteString.UTF8 (toString, fromString)
import Day8.AST (IncrementStatement(..), BinOp(..), ConditionalStatement(..), BooleanExpression(..))

registerName :: Parser String
registerName = toString <$> takeWhile1 (inClass "a-z")

increment :: Parser Int
increment = choice [inc, dec]
 where
  inc = string (fromString "inc ") *> signed decimal
  dec = string (fromString "dec ") *> (negate <$> signed decimal)

statement :: Parser IncrementStatement
statement = IncrementStatement <$> (registerName <* space) <*> increment

comparisonOperator :: Parser BinOp
comparisonOperator = choice $ map
  makeOp
  [ (">=", GreaterOrEq)
  , (">" , Greater)
  , ("<=", LessOrEq)
  , ("<" , Less)
  , ("==", Equal)
  , ("!=", NotEqual)
  ]
  where makeOp (token, f) = f <$ string (fromString token)

condition :: Parser BooleanExpression
condition =
  BooleanExpression
    <$> (registerName <* space)
    <*> (comparisonOperator <* space)
    <*> signed decimal

instruction :: Parser ConditionalStatement
instruction =
  flip ConditionalStatement
    <$> (statement <* string (fromString " if "))
    <*> (condition <* endOfLine)

instructionList :: Parser [ConditionalStatement]
instructionList = many' instruction <* endOfInput
