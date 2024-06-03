module Command (Command (..)) where

import Data.Text (Text)
import Expression (Expression (..))

data Command
  = AssignIdentifier Text Expression
  | EvaluateExpression Expression
  | PrintContext (Maybe Text)
  deriving (Eq, Show)