module Parser where 

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.SExpresso.Parse.Generic 

import Types 

-- numbers = L.decimal `sepBy` char ',' 
-- main = case parse numbers "" "11,2,43" of
--          Left bundle -> print "wrong bro"
--          Right xs -> print (sum xs)



atom = some letterChar

sexp = decode $ plainSExprParser atom

-- Returns (SList () [SAtom "hello", SAtom "world"])
ex1 = parse sexp "" "(hello world)"

-- Returns (SList () [SAtom "hello", SAtom "world", SList () [SAtom "bonjour"]])
ex2 = parse sexp "" "  (hello world(bonjour))  "

-- Returns SAtom "hola"
ex3 = parse sexp "" "hola"