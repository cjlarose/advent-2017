module Day8.AST where

type RegisterName = String
data BinOp = Greater | Less | GreaterOrEq | LessOrEq | Equal | NotEqual deriving (Show)
data BooleanExpression = BooleanExpression RegisterName BinOp Int deriving (Show)
data IncrementStatement = IncrementStatement RegisterName Int deriving (Show)
data ConditionalStatement = ConditionalStatement BooleanExpression IncrementStatement deriving (Show)
type Program = [ConditionalStatement]
