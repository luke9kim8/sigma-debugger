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


runPass :: Int -> String -> IO ()
runPass seed path = do
  smt <- readFile path 
  let Right sexps = runParser parseSmtSexprs "" smt
      sexps1 = contractFnNames sexps
      sexps2 = traverseInorder sexps1 (addZero . multOne)

  sexps3 <- evalSigmaM
    (runUntilFixedPoint sexps2 performRandomRemovalOnce) () seed
  writeFile ("smt/out_" ++ show seed ++ ".smt2") (fmtSmt sexps3)


