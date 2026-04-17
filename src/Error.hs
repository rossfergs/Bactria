module Error where

import Control.Exception (Exception)

data Error
  = InterpreterError String
  | ParserError String
  | LexerError String
  deriving (Show)

instance Exception Error
