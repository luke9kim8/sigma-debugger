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

tryRemoveAtom :: MonadIO m => Focus -> m Position
tryRemoveAtom (p@(Pos l _ r), hist) = do
  let newPos = Pos l (SAtom Empty) r
      newFocus = (newPos, hist)

  smtResult <- liftIO $ isValidSMT . rebuild $ newFocus
  return $ case smtResult of
    Success _ -> newPos
    Err _     -> p

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


--testMutation
--  :: [SmtSexp]
--  -> Sigma [SmtSexp]
--testMutation sexps = evalStateT m initFocus
--  where
--    initFocus = smtSexprsToFocus sexps
--    m = traverseZipperState (performRandomly removeAtom (return ()))

runUntilFixedPoint :: Monad m => [SmtSexp] -> ([SmtSexp] -> m [SmtSexp]) -> m [SmtSexp]
runUntilFixedPoint roots go = do
  updatedRoots <- go roots
  if updatedRoots == roots then return roots else runUntilFixedPoint updatedRoots go
