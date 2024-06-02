module Evaluator (Expression (..), evaluate) where

data Expression
  = Literal Double
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  deriving (Eq, Show)

evaluate :: Expression -> Double
evaluate (Literal n) = n
evaluate (Addition x y) = evaluate x + evaluate y
evaluate (Subtraction x y) = evaluate x - evaluate y
evaluate (Multiplication x y) = evaluate x * evaluate y
evaluate (Division x y) = evaluate x / evaluate y