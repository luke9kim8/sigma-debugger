module Parser
  ( module Parser
  , module Text.Megaparsec
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Char
import Data.SExpresso.Parse.Generic

import SmtSexp

-- numbers = L.decimal `sepBy` char ',' 
-- main = case parse numbers "" "11,2,43" of
--          Left bundle -> print "wrong bro"
--          Right xs -> print (sum xs)

parseSmtSexprs :: Parsec () String [SmtSexp]
parseSmtSexprs = decode $ plainSExprParser parseAtom

parseAtom :: Parsec () String Atom
parseAtom = p "=" Eq
  <|> p "true"  (B True) -- to not interfere with Fn matching
  <|> p "false" (B False)
  <|> p "and" And
  <|> p "not" Not
  <|> p "or" Or
  <|> p "<=" Leq
  <|> p ">=" Geq
  <|> p "<" Le
  <|> p ">" Ge
  <|> p "-" Neg
  <|> p "+" Add
  <|> p "*" Mult
  <|> p "assert" Assert
  <|> (some digitChar >>= (return . Val) . read)
  <|> (some (alphaNumChar <|> oneOf "!^_") >>= return . Fn)
  where
    p str atom = string str >> pure atom

--atom = some letterChar
--
--sexp = decode $ plainSExprParser atom
--
---- Returns (SList () [SAtom "hello", SAtom "world"])
--ex1 = parse sexp "" "(hello world)"
--
---- Returns (SList () [SAtom "hello", SAtom "world", SList () [SAtom "bonjour"]])
--ex2 = parse sexp "" "  (hello world(bonjour))  "
--
---- Returns SAtom "hola"
--ex3 = parse sexp "" "hola"