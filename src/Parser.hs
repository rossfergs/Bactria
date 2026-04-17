{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import Control.Exception (throw)
import Data.List as List
import Error (Error (ParserError))
import Lexer

data Sexp
  = List [Sexp]
  | Atom String
  deriving (Show)

parseAtom :: Token -> Sexp
parseAtom Token {lexeme} = Atom lexeme

parseList :: [Token] -> [Sexp] -> ([Sexp], [Token])
parseList [] _ = throw (ParserError "Unclosed list...")
parseList (Token {lexeme = ")"} : ts) acc = (List.reverse acc, ts)
parseList (Token {lexeme = "("} : ts) acc = parseList remaining_tokens (List sexps : acc)
  where
    (sexps, remaining_tokens) = parseList ts []
parseList (t : ts) acc = parseList ts (parseAtom t : acc)

parseImpl :: [Token] -> [Sexp]
parseImpl (Token {lexeme = ")", tok_line = line, tok_col = col} : _) =
  throw (ParserError ("Unmatched close brace at " ++ show (line, col)))
parseImpl (Token {lexeme = "("} : ts) =
  let (sexps, remaining_tokens) = parseList ts []
   in List sexps : parseImpl remaining_tokens
parseImpl (t : ts) = parseAtom t : parseImpl ts
parseImpl [] = []

parse :: String -> [Sexp]
parse input =
  let tokens = Lexer.tokenise input
   in parseImpl tokens
