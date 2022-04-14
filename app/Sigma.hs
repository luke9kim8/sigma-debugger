module Sigma where

import System.Process
import System.IO
import Parser
import Printer
import Mutators
import Traversal
import SmtSexp
import System.Exit


runPass :: String -> IO ()
runPass path = do
  smt <- readFile path 
  let Right sexps = runParser parseSmtSexprs "" smt
      sexps' = contractFnNames sexps
      --sexps'' = traverseInorder sexps'
      --  (addZero . multOne)
  sexps'' <- testMutation sexps' 
  writeFile "smt/out.smt2" (fmtSmt sexps'')

