module Evaluator
  ( Expression (..),
    Context,
    mkContext,
    constant,
    updateAnswer,
    evaluate,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T

data Expression
  = Literal Double
  | Constant Text
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  deriving (Eq, Show)

evaluate :: Context -> Expression -> Maybe Double
evaluate c (Literal n) = Just n
evaluate c (Constant n) = M.lookup n c
evaluate c (Addition x y) = (+) <$> evaluate c x <*> evaluate c y
evaluate c (Subtraction x y) = (-) <$> evaluate c x <*> evaluate c y
evaluate c (Multiplication x y) = (*) <$> evaluate c x <*> evaluate c y
evaluate c (Division x y) = (/) <$> evaluate c x <*> evaluate c y

type Context = Map Text Double

type Constant = (Text, Double)

mkContext :: [Maybe Constant] -> Context
mkContext = M.fromList . catMaybes

updateAnswer :: Context -> Double -> Context
updateAnswer = flip (M.insert $ T.pack "ans")

constant :: String -> Double -> Maybe Constant
constant k v | not $ null k = Just (T.pack k, v)
constant _ _ = Nothing