module Interpreter where

import Control.Exception (throw)
import Data.List as List
import Error (Error (InterpreterError))
import Parser

stringToOp :: String -> (Integer -> Integer -> Integer)
stringToOp op = case op of
  "+" -> (+)
  "-" -> (-)
  "*" -> (*)
  _ -> throw (InterpreterError op)

evaluateList :: [Sexp] -> String
evaluateList [] = throw (InterpreterError "evaluateList cannot be given empty list")
evaluateList (List _ : _) = throw (InterpreterError "a list operation cannot start with a list")
evaluateList [Atom _] = throw (InterpreterError "a must have values to operate on")
evaluateList (Atom op : ss)
  | op `notElem` ["+", "-", "*"] = throw (InterpreterError op)
  | otherwise =
      show (List.foldl (stringToOp op) firstValue values)
  where
    allValues = List.map (read . evaluate) ss
    (firstValue, values) = (head allValues, tail allValues)

evaluate :: Sexp -> String
evaluate (Atom a) = a
evaluate (List ss) = evaluateList ss

interpret :: String -> [String]
interpret = map evaluate . parse
