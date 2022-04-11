module Sigma where

import System.Process
import System.IO

import Parser
import Printer
import Mutators
import Traversal


runPass :: IO ()
runPass = do
  smt <- readFile "smt/static/d.smt2"
  let Right sexps = runParser parseSmtSexprs "" smt
      sexps' = contractFnNames sexps
      --sexps'' = traverseInorder sexps'
      --  (addZero . multOne)
  writeFile "smt/out.smt2" (fmtSmt sexps')
  putStrLn "Written to smt/out.smt2"
