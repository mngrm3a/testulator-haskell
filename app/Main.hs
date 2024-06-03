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
import Data.Text (Text)
import Data.Text qualified as T
import Expression (Expression, evaluate)
import Parser (parse)
import System.Console.Haskeline (CompletionFunc, InputT)
import System.Console.Haskeline qualified as Console
import Text.Printf qualified as T (printf)
import Tokenizer (tokenize)

main :: IO ()
main = do
  contextRef <- newIORef defaultContext
  runReaderT (Console.runInputT (mkHaskelineSettings contextRef) mainLoop) contextRef
  where
    mkHaskelineSettings env =
      Console.setComplete (completeIdentifier2 env) Console.defaultSettings

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
      Just description -> msgInfo $ pp description
      _ -> msgErr "unknown identifier"
    Nothing -> forM_ (getAllDescriptions context) (msgInfo . pp)
  where
    pp description = case description of
      ConstantDescription name value -> T.printf "const %s=%f" name value
      VariableDescription name value -> T.printf "var   %s=%f" name value
      FunctionDescription name arity -> T.printf "fun:%d %s" arity name

assignIdentifier :: Text -> Expression -> RIOT ()
assignIdentifier identifier expression = do
  context <- readContext
  case evaluate context expression of
    (Just value) ->
      case mkVariable identifier value >>= flip insertEntry context of
        Just context' -> do
          writeContext context'
          msgReply identifier value
        Nothing -> msgErr $ T.printf "identifier '%s' exists" identifier
    Nothing -> msgErr "invalid expression"

evaluateExpression :: Expression -> RIOT (Maybe Double)
evaluateExpression expression = do
  context <- readContext
  case evaluate context expression of
    (Just value) -> do
      writeContext $ insertAnswer value context
      msgReply ans value
      pure $ Just value
    Nothing -> msgErr "invalid expression" >> pure Nothing
  where
    ans = T.pack "ans"

msgReply :: (MonadIO m) => Text -> Double -> InputT m ()
msgReply name = Console.outputStrLn . T.printf "=> %s = %f" name

msgInfo, msgErr :: (MonadIO m) => String -> InputT m ()
msgInfo = Console.outputStrLn . T.printf ":> %s"
msgErr = Console.outputStrLn . T.printf "!> %s"

type Env = IORef Context

type RIOT = InputT (ReaderT Env IO)

readContext :: RIOT Context
readContext = lift ask >>= (liftIO . readIORef)

writeContext :: Context -> RIOT ()
writeContext context = lift ask >>= (liftIO . flip writeIORef context)

completeIdentifier2 :: (MonadIO m) => IORef Context -> CompletionFunc m
completeIdentifier2 contextRef =
  Console.completeWord' Nothing C.isSpace $ \input -> do
    descriptions <- getAllDescriptions <$> liftIO (readIORef contextRef)
    let (beforeBracket, afterBracket) = T.breakOnEnd bracketOpen $ T.pack input
    pure $ buildCompletion beforeBracket <$> findMatches descriptions afterBracket
  where
    buildCompletion prefix (name, completer) = completer $ prefix <> name
    findMatches descriptions input
      | T.null input = mkCompleter <$> descriptions
      | otherwise = filter (T.isPrefixOf input . fst) $ mkCompleter <$> descriptions
    mkCompleter description = case description of
      ConstantDescription name _ -> (name, \replacement -> mkCompletion replacement name True)
      VariableDescription name _ -> (name, \replacement -> mkCompletion replacement name True)
      FunctionDescription name _ -> (name, \replacement -> mkCompletion (replacement <> bracketOpen) name False)
    mkCompletion replacement display isFinished =
      Console.Completion
        { Console.replacement = T.unpack replacement,
          Console.display = T.unpack display,
          Console.isFinished = isFinished
        }
    bracketOpen = T.pack "("