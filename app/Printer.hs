module Printer where

import qualified Data.Set as S
import Control.Monad.State.Lazy
import Control.Monad

import Traversal
import SmtSexp
getFnNames :: [SmtSexp] -> [String]
getFnNames sexps = S.toList $ execState (traverseInorderM sexps unionFn) mempty
  where
    unionFn :: SmtSexp -> State (S.Set String) SmtSexp
    unionFn sexp =
      case sexp of
        x@(SAtom (Fn fn)) -> modify (S.insert fn) >> pure x
        x -> pure x

fmtSmtSexp :: SmtSexp -> String
fmtSmtSexp (SAtom atom) = show atom
fmtSmtSexp (SList () sexps) = let sexps' = filter ((/=) (SAtom Empty)) sexps in
  if null sexps' then ""
  else "(" ++ (unwords . map fmtSmtSexp) sexps' ++ ")"

fmtSmt :: [SmtSexp] -> String
fmtSmt sexps =
  "(set-info :smt-lib-version 2.6)\n"
  ++ "(set-logic ALL)\n"
  ++ "(set-info :category \"industrial\")\n"
  ++ "(set-info :status sat)\n"
  ++ concatMap
      (\fn -> "(declare-fun " ++ fn ++ " () Int)\n")
      fnNames
  ++ concatMap (\s -> fmtSmtSexp s ++ "\n") sexps
  ++ "(check-sat)\n"
  ++ "(exit)\n"
  where
    fnNames = getFnNames sexps