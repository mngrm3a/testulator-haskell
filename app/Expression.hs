module Expression
  ( Expression (..),
    Context,
    SymbolDescription (..),
    evaluate,
    mkContext,
    assignValue,
    updateAnswer,
    constant,
    unaryFunction,
    binaryFunction,
    symbolDescriptions,
    symbolDescription,
  )
where

import Data.Char qualified as C
import Data.List qualified as L
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
updateAnswer ctx = assignValue ctx (T.pack "ans")

assignValue :: Context -> Text -> Double -> Context
assignValue ctx n v = M.insert n (constantMapping v) ctx

symbolDescription :: Context -> Text -> Maybe SymbolDescription
symbolDescription ctx n = mkSymbolDescription n <$> M.lookup n ctx

symbolDescriptions :: Context -> [SymbolDescription]
symbolDescriptions = map snd . L.sortOn fst . M.foldrWithKey go []
  where
    go n m@(Mapping a _) xs = (a, mkSymbolDescription n m) : xs

data SymbolDescription
  = ValueDescription Text Double
  | FunctionDescription Text Int
  deriving (Eq, Show)

mkSymbolDescription :: Text -> Mapping -> SymbolDescription
mkSymbolDescription n (Mapping 0 f) = ValueDescription n $ f []
mkSymbolDescription n (Mapping a _) = FunctionDescription n a

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