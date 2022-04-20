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
import Traversal

type Mutator = SmtSexp -> SmtSexp
data SMTResult = Err String | Success String

flattenSmtSexp :: [SmtSexp] -> SmtSexp
flattenSmtSexp sexps = SmtList [SAtom Assert, SmtList (SAtom And:merged)]
  where
    f (SmtList ((SAtom Assert):xs)) = xs
    merged = concatMap f sexps

addZero :: Mutator
addZero (SmtList [SAtom Add, SAtom (Val 0), atom]) = atom
addZero (SmtList [SAtom Add, atom, SAtom (Val 0)]) = atom
addZero x = x

multOne :: Mutator
multOne (SmtList [SAtom Mult, SAtom (Val 1), atom]) = atom
multOne (SmtList [SAtom Mult, atom, SAtom (Val 1)]) = atom
multOne x = x

contractFnNames :: [SmtSexp] -> [SmtSexp]
contractFnNames roots = traverseInorder roots mut
  where
    mut :: SmtSexp -> SmtSexp
    mut (SAtom (Fn fn)) = SAtom (Fn (table M.! fn))
    mut x = x

    fnNames :: [String]
    fnNames =  map (:[]) ['A'..'Z'] ++ f 0
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

printSexps :: [SmtSexp] -> IO ()
printSexps sexps = traverseZipper sexps printAllFocus >> return ()
  where
    printAllFocus :: Focus -> IO Position
    printAllFocus (p@(Pos _ c _), _) = do
      liftIO $ print c >> putStrLn ">>= ::<u8>"
      return p

tryMutator :: MonadIO m => Mutator -> (Focus -> m Position)
tryMutator mut (oldPos@(Pos l sexp r), hist) = liftIO $ do
  let newPos = Pos l (mut sexp) r
      newFocus = (newPos, hist)

  smtResult <- isValidSMT . rebuild $ newFocus
  return $ case smtResult of
    Success _ -> newPos
    Err     _ -> oldPos

tryRemoveAtom :: MonadIO m => Focus -> m Position
tryRemoveAtom = tryMutator (const (SAtom Empty))

isValidSMT :: MonadIO m => [SmtSexp] -> m SMTResult
isValidSMT exprs = liftIO $ do
  writeFile "smt/test_out.smt2" (fmtSmt exprs)
  code <- system "bash b.sh smt/test_out.smt2 > /dev/null"
  if code == ExitSuccess then print "Success" >> return (Success "yass")
  else print "Unsuccsess" >> return (Err (show code))

performRemovalOnce :: MonadIO m => [SmtSexp] -> m [SmtSexp]
performRemovalOnce roots = traverseZipper roots tryRemoveAtom

performRandomRemovalOnce
  :: (MonadIO m, SigmaRandom m)
  => [SmtSexp]
  -> m [SmtSexp]
performRandomRemovalOnce roots = traverseZipper roots k
  where
    k focus = join $ performRandomly
      (tryRemoveAtom focus) (return (fst focus))

runUntilFixedPoint :: Monad m => [SmtSexp] -> ([SmtSexp] -> m [SmtSexp]) -> m [SmtSexp]
runUntilFixedPoint roots go = do
  updatedRoots <- go roots
  if updatedRoots == roots then return roots else runUntilFixedPoint updatedRoots go

