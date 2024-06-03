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
import Control.Monad (forM_, (>=>))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Expression (evaluate)
import Parser (parse)
import System.IO (hFlush, stdout)
import Tokenizer (tokenize)

main :: IO ()
main = mainLoop defaultContext

mainLoop :: Context -> IO ()
mainLoop context = do
  msgAsk
  command' <- T.getLine <&> (tokenize >=> parse)
  case command' of
    Just (PrintContext identifier') ->
      printContext identifier' context >> mainLoop context
    Just (Assignment identifier expression) -> do
      case evaluate context expression of
        Just value -> case mkVariable identifier value >>= flip insertEntry context of
          Just context' -> msgReply identifier value >> mainLoop context'
          _ -> msgErr ("identifier '" <> identifier <> "' exists") >> mainLoop context
        _ -> msgErr "invalid expression" >> mainLoop context
    Just (Expression expression) -> case evaluate context expression of
      (Just value) -> msgReply "ans" value >> mainLoop context
      _ -> msgErr "invalid expression" >> mainLoop context
    _ -> msgErr "invalid input" >> mainLoop context

printContext :: Maybe Text -> Context -> IO ()
printContext identifier' context = case identifier' of
  Just identifier -> case getDescriptionFor identifier context of
    Just description -> msgInfo $ prettyDescription description
    _ -> msgErr "unknown identifier"
  Nothing -> forM_ (getAllDescriptions context) (msgInfo . prettyDescription)
  where
    prettyDescription (ConstantDescription name value) = prettyValue "const " name value
    prettyDescription (VariableDescription name value) = prettyValue "var   " name value
    prettyDescription (FunctionDescription name arity) = "fun:" <> T.pack (show arity) <> " " <> name
    prettyValue prefix name value = prefix <> name <> " = " <> T.pack (show value)

msgAsk :: IO ()
msgAsk = T.putStr "?> " >> hFlush stdout

msgReply :: Text -> Double -> IO ()
msgReply name value = T.putStrLn $ "=> " <> name <> " = " <> T.pack (show value)

msgInfo, msgErr :: Text -> IO ()
msgInfo = T.putStrLn . (<>) ":> "
msgErr = T.putStrLn . (<>) "!> "