{-# LANGUAGE ScopedTypeVariables #-}
module Traversal where

import SmtSexp

import Data.List
import Control.Monad.State.Lazy
import Control.Monad

import Data.SExpresso.SExpr

data Crumb = Crumb [SmtSexp] [SmtSexp]
data Position = Pos [SmtSexp] SmtSexp [SmtSexp]
type Breadcrumbs = [Crumb]
type Focus = (Position, Breadcrumbs)

canMoveRight :: Focus -> Bool
canMoveRight (Pos _ _ rs, _) = not . null $ rs

canMoveUp :: Focus -> Bool
canMoveUp (_, hist) = not . null $ hist

moveRightSmtList :: Focus -> Focus
moveRightSmtList (Pos ls curr (r:rs), hist) = (Pos (ls ++ [curr]) r rs, hist)

moveUp :: Focus -> Focus
moveUp (Pos posL curr posR, Crumb cL cR:hs) = (Pos cL smtList posR, hs)
  where
    smtList = SmtList (posL ++ [curr] ++ posR)

moveDown :: Focus -> Focus
moveDown (Pos ls (SmtList (x:xs)) rs, hs) = (Pos [] x xs, Crumb ls rs:hs)

rebuild :: Focus -> SmtSexp
rebulid (curr, []) = curr
rebuild focus = rebuild (moveUp focus)

--traverseLevelOrderZipperM
--  :: forall a s m. (Eq a, Monad m)
--  => [Sexp a]
--  -> (Focus -> m ())
--  -> m ()
--traverseLevelOrderZipperM r:roots cps =
--  where
--    initPos = Pos [] r roots
--
--    levelOrder :: Focus -> m ()
--    levelOrder currFocus = do
--

traverseInorderM
  :: forall a s m. (Eq a, Monad m)
  => [Sexp a]
  -> (Sexp a -> m (Sexp a))
  -> m [Sexp a]
traverseInorderM roots cps = mapM inorder roots
  where
    inorder :: Sexp a -> m (Sexp a)
    inorder currRoot = do
      case currRoot of
        SAtom a -> cps $ SAtom a
        SList () sexps -> do
          sexps' <- mapM inorder sexps
          cps (SList () sexps')

traverseInorder
  :: forall a. Eq a
  => [Sexp a]
  -> (Sexp a -> Sexp a)
  -> [Sexp a]
traverseInorder roots cps = evalState (traverseInorderM roots cps') ()
  where
    cps' :: Sexp a -> State () (Sexp a)
    cps' s = return (cps s)