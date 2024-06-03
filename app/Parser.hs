{-# LANGUAGE LambdaCase #-}

module Parser (parse, parse') where

import Command (Command (..))
import Control.Applicative (Alternative (many, (<|>)), optional, (<**>))
import Control.Monad.State (StateT (StateT, runStateT))
import Data.Functor (void)
import Data.Text (Text)
import Expression (Expression (..))
import Tokenizer (Token (..))

parse :: [Token] -> Maybe Command
parse ts = case parse' ts of
  Just (c, []) -> Just c
  _ -> Nothing

parse' :: [Token] -> Maybe (Command, [Token])
parse' = runStateT command

command :: Parser Command
command = printContextOf <|> printContext <|> assignIdentifier <|> evaluateExpression
  where
    printContextOf = PrintContext . Just <$> (identifier <* eol)
    printContext = PrintContext <$ token QuestionMark <*> optional identifier
    assignIdentifier = AssignIdentifier <$> identifier <*> (token Equals *> expression)
    evaluateExpression = EvaluateExpression <$> expression

expression :: Parser Expression
expression = term <**> foldOps (plus <|> minus) term

term :: Parser Expression
term = factor <**> foldOps (star <|> slash) factor

factor :: Parser Expression
factor = brackets expression <|> function <|> constant <|> literal

function :: Parser Expression
function = Function <$> identifier <*> brackets args
  where
    args = (:) <$> expression <*> many (comma *> expression)
    comma = void $ token Comma

constant :: Parser Expression
constant = Constant <$> identifier

identifier :: Parser Text
identifier = satisfy $ \case
  (Identifier n) -> Just n
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

brackets :: Parser a -> Parser a
brackets p = token BracketOpen *> p <* token BracketClose

foldOps :: Parser (a -> a -> a) -> Parser a -> Parser (a -> a)
foldOps pOp pExpr = foldl (.) id . reverse <$> many ((flip <$> pOp) <*> pExpr)

type Parser = StateT [Token] Maybe

token :: Token -> Parser Token
token t = satisfy $ \t' -> if t == t' then Just t' else Nothing

satisfy :: (Token -> Maybe a) -> Parser a
satisfy p = StateT $ \case
  (t : ts) -> case p t of
    Just a -> Just (a, ts)
    _ -> Nothing
  _ -> Nothing

eol :: Parser ()
eol = StateT $ \case
  [] -> pure ((), [])
  _ -> Nothing