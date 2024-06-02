module Tokenizer (Token (..), tokenize, tokenize') where

import Control.Applicative (Alternative (many, some, (<|>)))
import Control.Monad.State (StateT (..))
import Data.Char qualified as C (isAlpha, isDigit, isSpace)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T

data Token
  = Number Double
  | Identifier Text
  | Plus
  | Minus
  | Star
  | Slash
  | Comma
  | BracketOpen
  | BracketClose
  deriving (Eq, Show)

tokenize :: Text -> Maybe [Token]
tokenize = fmap fst . tokenize'

tokenize' :: Text -> Maybe ([Token], Text)
tokenize' = runStateT (many $ skipSpaces *> token)
  where
    token =
      number
        <|> identifier
        <|> plus
        <|> minus
        <|> star
        <|> slash
        <|> comma
        <|> bracketOpen
        <|> bracketClose

skipSpaces :: Tokenizer ()
skipSpaces = void $ many $ satisfy C.isSpace

plus, minus, star, slash, comma, bracketOpen, bracketClose :: Tokenizer Token
plus = Plus <$ char '+'
minus = Minus <$ char '-'
star = Star <$ char '*'
slash = Slash <$ char '/'
comma = Comma <$ char ','
bracketOpen = BracketOpen <$ char '('
bracketClose = BracketClose <$ char ')'

identifier :: Tokenizer Token
identifier = Identifier . T.pack <$> some (satisfy C.isAlpha)

number :: Tokenizer Token
number = Number . read <$> (realNumber <|> number')
  where
    number' = some $ satisfy C.isDigit
    realNumber = stichRealNumber <$> number' <* char '.' <*> number'
    stichRealNumber n f = n <> ('.' : f)

type Tokenizer = StateT Text Maybe

char :: Char -> Tokenizer Char
char = satisfy . (==)

satisfy :: (Char -> Bool) -> Tokenizer Char
satisfy p = StateT $ \s -> case T.uncons s of
  Just (c, cs)
    | p c -> Just (c, cs)
    | otherwise -> Nothing
  Nothing -> Nothing