{-# LANGUAGE FlexibleContexts #-}

module Mutators where

import Traversal
import SmtSexp
import Printer
import qualified Data.Map as M
import Data.SExpresso.SExpr
import Control.Monad.State.Lazy
import System.Exit
import Debug.Trace
import System.Process
import System.Random.Stateful
import SigmaM

type Mutator = SmtSexp -> SmtSexp
data SMTResult = Err String | Success String

addZero :: Mutator
addZero (SmtList [SAtom Add, SAtom (Val 0), atom]) = atom
addZero (SmtList [SAtom Add, atom, SAtom (Val 0)]) = atom
addZero x = x

multOne :: Mutator
multOne (SmtList [SAtom Mult, SAtom (Val 1), atom]) = atom
multOne (SmtList [SAtom Mult, atom, SAtom (Val 1)]) = atom
multOne x = x

negateIneq :: Mutator
negateIneq s@(SmtList [SAtom Not, SmtList ((SAtom e) : xs)]) =
  case e of
    Leq -> SmtList $ SAtom Ge  : xs
    Geq -> SmtList $ SAtom Le  : xs
    Le  -> SmtList $ SAtom Geq : xs
    Ge  -> SmtList $ SAtom Leq : xs
    _ -> s
negateIneq s = s

zeroOnOneSide :: Mutator
zeroOnOneSide s@(SmtList
  [SAtom ineq
    , SmtList ((SAtom Add):(SAtom (Val k)):xs)
    , SAtom (Val 0)]) =
      if ineq `elem` [Eq, Leq, Geq, Le, Ge]
        then
          let k' = if k <= 0
                     then SAtom (Val (-k))
                     else SmtList [SAtom Neg, SAtom (Val k)]
          in SmtList [SAtom ineq, SmtList xs, k']
        else s
zeroOnOneSide x = x

contractFnNames :: [SmtSexp] -> [SmtSexp]
contractFnNames roots = traverseInorder roots mut
  where
    mut :: SmtSexp -> SmtSexp
    mut (SAtom (Fn fn)) = SAtom (Fn (table M.! fn))
    mut x = x

    fnNames :: [String]
    fnNames =  f 0
      where
        f idx = map (\ch -> ch:show idx) ['A'..'Z']
          ++ f (idx + 1)

    table :: M.Map String String
    table = traceShow k k
      where
        k = snd $ execState (traverseInorderM roots insFn)
          --(fnNames, M.singleton "lam5n8" "lam5n8")
          (fnNames, mempty)

    insFn :: SmtSexp -> State ([String], M.Map String String) SmtSexp
    insFn sexp =
      case sexp of
        x@(SAtom (Fn fn)) -> do
          (ns, tb) <- get
          unless (fn `M.member` tb) $
            put (drop 1 ns, M.insert fn (head ns) tb)
          return x
        x -> return x

type Sigma = SigmaM Focus

printAllFocus :: (MonadState Focus m, MonadIO m) => m ()
printAllFocus = do
  (Pos _ c _, _) <- get
  liftIO $ print c >> putStrLn ">>= ::<u8>"
  return ()

printSexps :: [SmtSexp] -> IO ()
printSexps sexps = do
  runStateT m initFocus >> pure ()
  where
    initFocus = smtSexprsToFocus sexps
    m = traverseZipperState printAllFocus

removeAtom :: (MonadState Focus m, MonadIO m) => m ()
removeAtom = do
  (Pos l _ r, hist) <- get
  let newSMT = (Pos l (SAtom Empty) r, hist)
  smtResult <- liftIO $ testSMT . rebuild $ newSMT
  case smtResult of 
    Success _ -> put $ newSMT
    Err _     -> return ()
  
testMutation
  :: [SmtSexp]
  -> Sigma [SmtSexp]
testMutation sexps = evalStateT m initFocus
  where
    initFocus = smtSexprsToFocus sexps
    m = traverseZipperState (performRandomly removeAtom (return ()))

runTestMutationUntilFixedPoint :: (MonadIO m, SigmaRandom m) => [SmtSexp] -> m [SmtSexp]
runTestMutationUntilFixedPoint sexps = do 
  sexps' <- testMutation sexps
  if sexps' == sexps 
  then return sexps'
  else runTestMutationUntilFixedPoint sexps'

testSMT :: [SmtSexp] -> IO SMTResult
testSMT exprs = do
  writeFile "smt/test_out.smt2" (fmtSmt exprs)
  code <- system "bash b.sh smt/test_out.smt2 > /dev/null"
  if code == ExitSuccess then print "Success" >> return (Success "yass")
  else print "Unsuccsess" >> return (Err (show code))

