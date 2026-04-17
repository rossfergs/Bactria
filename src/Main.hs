module Main where

import Control.Exception
import qualified Data.Foldable as List
import Debug.Trace (trace)
import Error
import Interpreter

eval :: IO ()
eval = do
  exprs <- getLine
  let result = List.foldl (\acc r -> acc ++ r ++ "  ") "" (interpret exprs)
  putStrLn result
  eval

repl :: IO ()
repl = do
  putStr " ==> "
  result <- try eval
  case result of
    Left (InterpreterError msg) -> trace ("Runtime Error: " ++ msg) repl
    Left (ParserError msg) -> trace ("Parser Error: " ++ msg) repl
    Left (LexerError msg) -> trace ("Lexer Error: " ++ msg) repl
    Right () -> repl

main :: IO ()
main = do
  putStrLn "Welcome to the Bactria Interpreter!"
  repl
