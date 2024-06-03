module Expression
  ( Expression (..),
    evaluate,
  )
where

import Context (Context, lookupFunction, lookupValue)
import Data.Text (Text)

data Expression
  = Literal Double
  | Constant Text
  | Function Text [Expression]
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  deriving (Eq, Show)

evaluate :: Context -> Expression -> Maybe Double
evaluate _ (Literal n) = Just n
evaluate c (Constant n) = lookupValue n c
evaluate c (Function n as) =
  lookupFunction n (length as) c <*> traverse (evaluate c) as
evaluate c (Addition x y) = (+) <$> evaluate c x <*> evaluate c y
evaluate c (Subtraction x y) = (-) <$> evaluate c x <*> evaluate c y
evaluate c (Multiplication x y) = (*) <$> evaluate c x <*> evaluate c y
evaluate c (Division x y) = (/) <$> evaluate c x <*> evaluate c y
