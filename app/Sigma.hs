module Sigma where

import System.Process
import System.IO
import Parser
import Printer
import Mutators
import Traversal
import SmtSexp
import System.Exit
import System.Random.Stateful
import SigmaM 


runPass :: String -> IO ()
runPass path = do
  smt <- readFile path 
  let Right sexps = runParser parseSmtSexprs "" smt
      sexps1 = contractFnNames sexps
      sexps2 = traverseInorder sexps1
        (addZero . multOne )
  let pureGen = mkStdGen 100
  sexps3 <- evalSigmaM (runTestMutationUntilFixedPoint sexps2) () 100
  writeFile "smt/out.smt2" (fmtSmt sexps3)


