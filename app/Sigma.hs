module Sigma where

import System.Process
import System.IO
import Parser
import Printer
import Mutators
import Traversal
import SmtSexp
import System.Exit

data SMTResult = Err String | Success String

runPass :: String -> IO ()
runPass path = do
  smt <- readFile path 
  let Right sexps = runParser parseSmtSexprs "" smt
      sexps' = contractFnNames sexps
      --sexps'' = traverseInorder sexps'
      --  (addZero . multOne)
  writeFile "smt/out.smt2" (fmtSmt sexps')
  putStrLn "Written to smt/out.smt2" 

testSMT :: [SmtSexp] -> IO SMTResult
testSMT exprs = do
  writeFile "smt/test.out" (fmtSmt exprs)
  code <- system "bash b.sh smt/test.out > bash_ouput.out"
  cvcOutput <- readFile "tmp.out"
  if code == ExitSuccess then return $ Success cvcOutput
  else return $ Err (show code)
