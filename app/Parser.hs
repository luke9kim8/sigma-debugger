module Parser
  ( module Parser
  , module Text.Megaparsec
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.SExpresso.Parse.Char
import Data.SExpresso.Parse.Generic
import Debug.Trace

import SmtSexp

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
  <|> trace ("parsing function") (some (alphaNumChar <|> oneOf "!^_") >>= return . Fn)
  where
    p str atom = string str >> pure atom
