module Evaluator
  ( Expression (..),
    Context,
    mkContext,
    constant,
    updateAnswer,
    evaluate,
    unaryFunction,
    binaryFunction,
  )
where

import Data.Char qualified as C
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T

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
evaluate c (Constant n) = case M.lookup n c of
  Just (Mapping 0 f) -> Just $ f []
  _ -> Nothing
evaluate c (Function n as) = case M.lookup n c of
  Just (Mapping a f) | length as == a -> f <$> traverse (evaluate c) as
  _ -> Nothing
evaluate c (Addition x y) = (+) <$> evaluate c x <*> evaluate c y
evaluate c (Subtraction x y) = (-) <$> evaluate c x <*> evaluate c y
evaluate c (Multiplication x y) = (*) <$> evaluate c x <*> evaluate c y
evaluate c (Division x y) = (/) <$> evaluate c x <*> evaluate c y

type Context = Map Text Mapping

mkContext :: [Maybe (Text, Mapping)] -> Context
mkContext = M.fromList . catMaybes

updateAnswer :: Context -> Double -> Context
updateAnswer ctx v = M.insert (T.pack "ans") (constantMapping v) ctx

getNameAndArity :: Context -> [(Text, Int)]
getNameAndArity = M.foldrWithKey' go []
  where
    go n (Mapping a _) xs = (n, a) : xs

binaryFunction :: Text -> (Double -> Double -> Double) -> Maybe (Text, Mapping)
binaryFunction n f = mkEntry n <*> pure (binaryMapping f)

unaryFunction :: Text -> (Double -> Double) -> Maybe (Text, Mapping)
unaryFunction n f = mkEntry n <*> pure (unaryMapping f)

constant :: Text -> Double -> Maybe (Text, Mapping)
constant n v = mkEntry n <*> pure (constantMapping v)

mkEntry :: Text -> Maybe (Mapping -> (Text, Mapping))
mkEntry n
  | isValid = Just (n,)
  | otherwise = Nothing
  where
    isValid = not $ T.null n && T.all C.isAlpha n

data Mapping = Mapping Int ([Double] -> Double)

binaryMapping :: (Double -> Double -> Double) -> Mapping
binaryMapping f = Mapping 2 $ \(x : y : _) -> f x y

unaryMapping :: (Double -> Double) -> Mapping
unaryMapping f = Mapping 1 $ \(x : _) -> f x

constantMapping :: Double -> Mapping
constantMapping = Mapping 0 . const