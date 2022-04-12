{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module SmtSexp
  ( module SmtSexp
  , module Data.SExpresso.SExpr
  )where

import Data.SExpresso.SExpr

type SmtSexp = Sexp Atom
pattern SmtList xs = SList () xs

data Atom
  = Eq
  | Not
  | And
  | Or
  | Leq
  | Geq
  | Le
  | Ge
  | Neg
  | Add
  | Mult
  | Assert
  | Val Int
  | Fn String
  | B Bool
  | Empty  -- For delta-debugging purposes, when we want to remove a sexpr
  deriving (Eq)

instance Show Atom where
  show atom = case atom of
    Eq     -> "="
    And    -> "and"
    Not    -> "not"
    Or     -> "or"
    Leq    -> "<="
    Geq    -> ">="
    Le     -> "<"
    Ge     -> ">"
    Neg    -> "-"
    Add    -> "+"
    Mult   -> "*"
    Assert -> "assert"
    Val n  -> show n
    Fn fn  -> fn
    B b    -> if b then "true" else "false"
    Empty  -> ""
