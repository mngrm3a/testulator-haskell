module Main (main) where

import Data.Text.IO qualified as T
import Evaluator (evaluate)
import Parser (parse)
import Tokenizer (tokenize)

main :: IO ()
main = loop
  where
    loop = do
      input <- T.getLine
      print $ evaluate <$> (tokenize input >>= parse)
      loop
