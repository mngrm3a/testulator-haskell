{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Command (Command (..))
import Context
  ( Context,
    MappingDescription (..),
    defaultContext,
    getAllDescriptions,
    getDescriptionFor,
    insertEntry,
    mkVariable,
  )
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Expression (evaluate)
import Parser (parse)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as Console
import Tokenizer (tokenize)

main :: IO ()
main = Console.runInputT Console.defaultSettings $ mainLoop defaultContext

mainLoop :: Context -> InputT IO ()
mainLoop context = do
  input <- Console.getInputLine "?> "
  case (input >>= tokenize . T.pack) >>= parse of
    Just (PrintContext identifier') ->
      printContext identifier' context >> mainLoop context
    Just (Assignment identifier expression) -> do
      case evaluate context expression of
        Just value -> case mkVariable identifier value >>= flip insertEntry context of
          Just context' -> msgReply identifier value >> mainLoop context'
          _ -> msgErr ("identifier '" <> T.unpack identifier <> "' exists") >> mainLoop context
        _ -> msgErr "invalid expression" >> mainLoop context
    Just (Expression expression) -> case evaluate context expression of
      (Just value) -> msgReply "ans" value >> mainLoop context
      _ -> msgErr "invalid expression" >> mainLoop context
    _ -> msgErr "invalid input" >> mainLoop context

printContext :: Maybe Text -> Context -> InputT IO ()
printContext identifier' context = case identifier' of
  Just identifier -> case getDescriptionFor identifier context of
    Just description -> msgInfo $ prettyDescription description
    _ -> msgErr "unknown identifier"
  Nothing -> forM_ (getAllDescriptions context) (msgInfo . prettyDescription)
  where
    prettyDescription (ConstantDescription name value) = prettyValue "const " name value
    prettyDescription (VariableDescription name value) = prettyValue "var   " name value
    prettyDescription (FunctionDescription name arity) = "fun:" <> show arity <> " " <> T.unpack name
    prettyValue prefix name value = prefix <> T.unpack name <> " = " <> show value

msgReply :: Text -> Double -> InputT IO ()
msgReply name value = Console.outputStrLn $ "=> " <> T.unpack name <> " = " <> show value

msgInfo, msgErr :: String -> InputT IO ()
msgInfo = Console.outputStrLn . (<>) ":> "
msgErr = Console.outputStrLn . (<>) "!> "