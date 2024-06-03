{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Command (Command (..))
import Context
  ( Context,
    MappingDescription (..),
    defaultContext,
    getAllDescriptions,
    getDescriptionFor,
    insertAnswer,
    insertEntry,
    mkVariable,
  )
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), MonadTrans (..), ReaderT (..))
import Data.Char qualified as C
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Expression (Expression, evaluate)
import Parser (parse)
import System.Console.Haskeline (CompletionFunc, InputT)
import System.Console.Haskeline qualified as Console
import Tokenizer (tokenize)

main :: IO ()
main = do
  contextRef <- newIORef defaultContext
  runReaderT (Console.runInputT (mkHaskelineSettings contextRef) mainLoop) contextRef
  where
    mkHaskelineSettings env =
      Console.setComplete (completeIdentifier env) Console.defaultSettings

mainLoop :: RIOT ()
mainLoop = do
  input <- Console.getInputLine "?> "
  case (input >>= tokenize . T.pack) >>= parse of
    Just command -> case command of
      PrintContext identifier -> printContext identifier
      AssignIdentifier identifier expression -> assignIdentifier identifier expression
      EvaluateExpression expression -> void $ evaluateExpression expression
    _ -> msgErr "invalid input"
  mainLoop

printContext :: Maybe Text -> RIOT ()
printContext identifier' = do
  context <- lift ask >>= (liftIO . readIORef)
  case identifier' of
    Just identifier -> case getDescriptionFor identifier context of
      Just description -> msgInfo $ prettyDescription description
      _ -> msgErr "unknown identifier"
    Nothing -> forM_ (getAllDescriptions context) (msgInfo . prettyDescription)
  where
    prettyDescription (ConstantDescription name value) = prettyValue "const " name value
    prettyDescription (VariableDescription name value) = prettyValue "var   " name value
    prettyDescription (FunctionDescription name arity) = "fun:" <> show arity <> " " <> T.unpack name
    prettyValue prefix name value = prefix <> T.unpack name <> " = " <> show value

assignIdentifier :: Text -> Expression -> RIOT ()
assignIdentifier identifier expression = do
  context <- readContext
  case evaluate context expression of
    (Just value) ->
      case mkVariable identifier value >>= flip insertEntry context of
        Just context' -> do
          writeContext context'
          msgReply identifier value
        Nothing -> msgErr $ "identifier '" <> T.unpack identifier <> "' exists"
    Nothing -> msgErr "invalid expression"

evaluateExpression :: Expression -> RIOT (Maybe Double)
evaluateExpression expression = do
  context <- readContext
  case evaluate context expression of
    (Just value) -> do
      writeContext $ insertAnswer value context
      msgReply "ans" value
      pure $ Just value
    Nothing -> msgErr "invalid expression" >> pure Nothing

msgReply :: (MonadIO m) => Text -> Double -> InputT m ()
msgReply name value = Console.outputStrLn $ "=> " <> T.unpack name <> " = " <> show value

msgInfo, msgErr :: (MonadIO m) => String -> InputT m ()
msgInfo = Console.outputStrLn . (<>) ":> "
msgErr = Console.outputStrLn . (<>) "!> "

type Env = IORef Context

type RIOT = InputT (ReaderT Env IO)

readContext :: RIOT Context
readContext = lift ask >>= (liftIO . readIORef)

writeContext :: Context -> RIOT ()
writeContext context = lift ask >>= (liftIO . flip writeIORef context)

completeIdentifier :: (MonadIO m) => IORef Context -> CompletionFunc m
completeIdentifier contextRef =
  Console.completeWord' Nothing C.isSpace $ \input -> do
    identifiers <- getIdentifiers <$> liftIO (readIORef contextRef)
    pure $ Console.simpleCompletion <$> filter (L.isPrefixOf input) identifiers
  where
    getIdentifiers = fmap (T.unpack . getIdentifier) . getAllDescriptions
    getIdentifier (ConstantDescription name _) = name
    getIdentifier (VariableDescription name _) = name
    getIdentifier (FunctionDescription name _) = name
