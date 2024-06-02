{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.IO qualified as T
import Evaluator (Context, constant, evaluate, mkContext, unaryFunction, updateAnswer)
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

mainLoop :: Context -> IO a
mainLoop ctx = do
  putStr "> " >> hFlush stdout
  input <- T.getLine
  case tokenize input >>= parse >>= evaluate ctx of
    Just ans -> do
      putStrLn $ "= " <> show ans
      mainLoop $ updateAnswer ctx ans
    Nothing -> do
      putStrLn "! invalid expression"
      mainLoop ctx
