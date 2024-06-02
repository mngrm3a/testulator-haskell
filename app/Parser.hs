{-# LANGUAGE LambdaCase #-}

module Parser (parse, parse') where

import Control.Applicative (Alternative (many, (<|>)), (<**>))
import Control.Monad.State (StateT (StateT, runStateT))
import Evaluator (Expression (..))
import Tokenizer (Token (..))

parse :: [Token] -> Maybe Expression
parse = fmap fst . parse'

parse' :: [Token] -> Maybe (Expression, [Token])
parse' = runStateT expression

expression :: Parser Expression
expression = term <**> foldOps (plus <|> minus) term

term :: Parser Expression
term = factor <**> foldOps (star <|> slash) factor

factor :: Parser Expression
factor = brackets expression <|> constant <|> literal
  where
    brackets p = token BracketOpen *> p <* token BracketClose

constant :: Parser Expression
constant = satisfy $ \case
  (Identifier n) -> Just $ Constant n
  _ -> Nothing

literal :: Parser Expression
literal = satisfy $ \case
  (Number n) -> Just $ Literal n
  _ -> Nothing

plus, minus, star, slash :: Parser (Expression -> Expression -> Expression)
plus = Addition <$ token Plus
minus = Subtraction <$ token Minus
star = Multiplication <$ token Star
slash = Division <$ token Slash

foldOps :: Parser (a -> a -> a) -> Parser a -> Parser (a -> a)
foldOps pOp pExpr = foldl (.) id . reverse <$> many ((flip <$> pOp) <*> pExpr)

type Parser = StateT [Token] Maybe

token :: Token -> Parser Token
token t = satisfy $ tokenEq t
  where
    tokenEq (Number _) r@(Number _) = Just r
    tokenEq x y = if x == y then Just y else Nothing

satisfy :: (Token -> Maybe a) -> Parser a
satisfy p = StateT $ \cases
  (t : ts) -> case p t of
    Just a -> Just (a, ts)
    _ -> Nothing
  _ -> Nothing