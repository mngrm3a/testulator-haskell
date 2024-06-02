{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Command (Command (..))
import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Expression
  ( Context,
    SymbolDescription (..),
    assignValue,
    constant,
    evaluate,
    mkContext,
    symbolDescription,
    symbolDescriptions,
    unaryFunction,
    updateAnswer,
  )
import Parser (parse)
import System.IO (hFlush, stdout)
import Tokenizer (tokenize)

main :: IO ()
main = mainLoop context
  where
    context =
      mkContext
        [ unaryFunction "sin" sin,
          unaryFunction "cos" cos,
          unaryFunction "tan" tan,
          unaryFunction "pow" $ \x -> x * x,
          constant "pi" 3.14
        ]

mainLoop :: Context -> IO ()
mainLoop ctx = do
  putStr "?> " >> hFlush stdout
  input <- T.getLine
  case tokenize input >>= parse of
    (Just command) -> case command of
      PrintContext identifier' -> do
        case identifier' of
          Just identifier -> case symbolDescription ctx identifier of
            Just sd -> printPrettySymbol sd
            Nothing -> T.putStrLn "!> unknown identifier"
          Nothing -> forM_ (symbolDescriptions ctx) printPrettySymbol
        mainLoop ctx
      Assignment n e -> case evaluate ctx e of
        Just ans -> printNameAndContinue n ans $ assignValue ctx n ans
        Nothing -> printErrorAndContinue
      Expression e -> case evaluate ctx e of
        Just ans -> printNameAndContinue "ans" ans $ updateAnswer ctx ans
        Nothing -> printErrorAndContinue
    Nothing -> printErrorAndContinue
  where
    printNameAndContinue n v c = do
      T.putStrLn $ "=> " <> n <> " = " <> T.pack (show v)
      mainLoop c
    printErrorAndContinue = do
      putStrLn "!> invalid input"
      mainLoop ctx
    printPrettySymbol = T.putStrLn . prettySymbol
    prettySymbol (ValueDescription n v) =
      ":> const '" <> n <> "' = " <> T.pack (show v)
    prettySymbol (FunctionDescription n a) =
      ":> fun:" <> T.pack (show a) <> " '" <> n <> "'"
