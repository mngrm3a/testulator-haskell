module Main (main) where

import Data.Text.IO qualified as T
import Evaluator (Context, constant, evaluate, mkContext, updateAnswer)
import Parser (parse)
import System.IO (hFlush, stdout)
import Tokenizer (tokenize)

main :: IO ()
main = mainLoop $ mkContext [constant "pi" 3.14]

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
