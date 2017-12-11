module Day8.Parser (program) where

import Data.Attoparsec.Text (Parser, takeWhile1, inClass, choice, string, many', endOfInput, signed, decimal, space, endOfLine)
import Data.Text (pack, unpack)
import Day8.AST (IncrementStatement(..), BinOp(..), ConditionalStatement(..), BooleanExpression(..), Program)

registerName :: Parser String
registerName = unpack <$> takeWhile1 (inClass "a-z")

increment :: Parser Int
increment = choice [inc, dec]
 where
  inc = string (pack "inc ") *> signed decimal
  dec = string (pack "dec ") *> (negate <$> signed decimal)

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
  where makeOp (token, f) = f <$ string (pack token)

condition :: Parser BooleanExpression
condition =
  BooleanExpression
    <$> (registerName <* space)
    <*> (comparisonOperator <* space)
    <*> signed decimal

instruction :: Parser ConditionalStatement
instruction =
  flip ConditionalStatement
    <$> (statement <* string (pack " if "))
    <*> (condition <* endOfLine)

program :: Parser Program
program = many' instruction <* endOfInput
