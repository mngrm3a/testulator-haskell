module Command (Command (..)) where

import Data.Text (Text)
import Expression (Expression (..))

data Command
  = Assignment Text Expression
  | Expression Expression
  | PrintContext (Maybe Text)
  deriving (Eq, Show)