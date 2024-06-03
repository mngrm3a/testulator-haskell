{-# LANGUAGE OverloadedStrings #-}

module Context
  ( Context,
    mkContext,
    defaultContext,
    ContextEntry,
    mkConstant,
    mkVariable,
    mkUnaryFunction,
    mkBinaryFunction,
    insertEntry,
    insertAnswer,
    lookupValue,
    lookupFunction,
    MappingDescription (..),
    getDescriptionFor,
    getAllDescriptions,
  )
where

import Data.Char qualified as C
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Data.Text qualified as T

newtype Context = Context {unContext :: Map Text Mapping}

defaultContext :: Context
defaultContext =
  mkContext $
    catMaybes
      [ mkConstant "pi" 3.14,
        mkUnaryFunction "sin" sin,
        mkUnaryFunction "cos" cos,
        mkUnaryFunction "tan" tan,
        mkUnaryFunction "sqrt" sqrt,
        mkUnaryFunction "exp" exp
      ]

data ContextEntry = ContextEntry !Text !Mapping

data Mapping
  = Constant !Double
  | Variable !Double
  | Function !Int ([Double] -> Double)

mkContext :: [ContextEntry] -> Context
mkContext = Context . M.fromList . filter ((/=) "ans" . fst) . map toTuple
  where
    toTuple (ContextEntry name mapping) = (name, mapping)

mkContext' :: [Maybe ContextEntry] -> Context
mkContext' = mkContext . catMaybes

mkEntry' :: Text -> Maybe (Mapping -> ContextEntry)
mkEntry' name
  | check name = Just $ ContextEntry name
  | otherwise = Nothing
  where
    check c = all ($ c) [not . T.null, isNothing . T.find C.isSpace, T.all C.isAlpha]

mkConstant :: Text -> Double -> Maybe ContextEntry
mkConstant "ans" _ = Nothing
mkConstant name value = mkEntry' name <*> pure (Constant value)

mkVariable :: Text -> Double -> Maybe ContextEntry
mkVariable name value = mkEntry' name <*> pure (Variable value)

mkUnaryFunction :: Text -> (Double -> Double) -> Maybe ContextEntry
mkUnaryFunction "ans" _ = Nothing
mkUnaryFunction name fun = mkEntry' name <*> pure (Function 1 $ \[x] -> fun x)

mkBinaryFunction :: Text -> (Double -> Double -> Double) -> Maybe ContextEntry
mkBinaryFunction "ans" _ = Nothing
mkBinaryFunction name fun = mkEntry' name <*> pure (Function 2 $ \(x : y : _) -> fun x y)

insertAnswer :: Double -> Context -> Context
insertAnswer value c = runIdentity $ withContext c $ \context ->
  Identity $ M.insert "ans" (Variable value) context

insertEntry :: ContextEntry -> Context -> Maybe Context
insertEntry (ContextEntry name mapping@(Variable _)) c = withContext c $ \context ->
  case M.insertLookupWithKey (\_ new _ -> new) name mapping context of
    (Just (Variable _), context') -> Just context'
    (Just _, _) -> Nothing
    (Nothing, context') -> Just context'
insertEntry (ContextEntry name mapping) c = withContext c $ \context ->
  case M.insertLookupWithKey (\_ new _ -> new) name mapping context of
    (Just _, _) -> Nothing
    (Nothing, context') -> Just context'

lookupValue :: Text -> Context -> Maybe Double
lookupValue name (Context context) = case M.lookup name context of
  Just (Constant value) -> Just value
  Just (Variable value) -> Just value
  _ -> Nothing

lookupFunction :: Text -> Int -> Context -> Maybe ([Double] -> Double)
lookupFunction name arity (Context context) = case M.lookup name context of
  Just (Function arity' function) | arity == arity' -> Just function
  _ -> Nothing

withContext :: (Functor f) => Context -> (Map Text Mapping -> f (Map Text Mapping)) -> f Context
withContext (Context context) g = Context <$> g context

data MappingDescription
  = VariableDescription Text Double
  | ConstantDescription Text Double
  | FunctionDescription Text Int
  deriving (Eq, Show, Ord)

mapNameAndmappingToDescription :: Text -> Mapping -> MappingDescription
mapNameAndmappingToDescription name (Variable value) = VariableDescription name value
mapNameAndmappingToDescription name (Constant value) = ConstantDescription name value
mapNameAndmappingToDescription name (Function arity _) = FunctionDescription name arity

getDescriptionFor :: Text -> Context -> Maybe MappingDescription
getDescriptionFor name (Context context) =
  mapNameAndmappingToDescription name <$> M.lookup name context

getAllDescriptions :: Context -> [MappingDescription]
getAllDescriptions (Context context) =
  L.sort (uncurry mapNameAndmappingToDescription <$> M.toList context)