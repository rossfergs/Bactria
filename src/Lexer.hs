module Lexer where

import qualified Data.List as List
import GHC.TypeLits (Nat)

data Token = Token
  { lexeme :: String,
    tok_line :: Nat,
    tok_col :: Nat
  }
  deriving (Show)

groupStrings :: String -> [Token]
groupStrings = impl [] [] (1, 1)
  where
    impl current acc (line, col) [] = List.reverse (flush current acc line (col - 1))
    impl current acc (line, col) (c : cs)
      | c == '\n' = impl [] (flush current acc line (col - 1)) (line + 1, 1) cs
      | c == ' ' = impl [] (flush current acc line (col - 1)) (line, col + 1) cs
      | c `elem` ['(', ')', '*', '+', '-'] =
          impl [] (makeToken [c] line col : flush current acc line (col - 1)) (line, col + 1) cs
      | otherwise = impl (c : current) acc (line, col + 1) cs
    flush [] a _ _ = a
    flush cur a l c = makeToken (List.reverse cur) l c : a
    makeToken text line col = Token {lexeme = text, tok_line = line, tok_col = col}

tokenise :: String -> [Token]
tokenise = groupStrings
