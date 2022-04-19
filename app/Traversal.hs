{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Traversal where

import SmtSexp

import Data.List
import Control.Monad.State.Lazy
import Control.Monad

import Data.SExpresso.SExpr

data Crumb = Crumb [SmtSexp] [SmtSexp] deriving Show
data Position = Pos [SmtSexp] SmtSexp [SmtSexp] deriving Show
type Breadcrumbs = [Crumb]
type Focus = (Position, Breadcrumbs)

canMoveRight :: Focus -> Bool
canMoveRight (Pos _ _ rs, _) = not . null $ rs

canMoveUp :: Focus -> Bool
canMoveUp (_, hist) = not . null $ hist

canMoveDown :: Focus -> Bool
canMoveDown (Pos _ (SmtList _) _, _) = True
canMoveDown x = False

posToSmtSexpr :: Position -> SmtSexp
posToSmtSexpr (Pos l curr r) = SmtList (l ++ [curr] ++ r)

smtSexprsToFocus :: [SmtSexp] -> Focus
smtSexprsToFocus (x:xs) = (Pos [] x xs, [])

moveRight :: Focus -> Focus
moveRight (Pos ls curr (r:rs), hist) = (Pos (ls ++ [curr]) r rs, hist)

moveUp :: Focus -> Focus
moveUp (currPos, Crumb cL cR:hs) = (Pos cL smtList cR, hs)
  where
    smtList = posToSmtSexpr currPos

moveDown :: Focus -> Focus
moveDown (Pos ls (SmtList (x:xs)) rs, hs) = (Pos [] x xs, Crumb ls rs:hs)

rebuild :: Focus -> [SmtSexp]
rebuild (curr, []) = let (SmtList xs) = posToSmtSexpr curr in xs
rebuild focus = rebuild (moveUp focus)

traverseZipper
  :: forall m. Monad m
  => [SmtSexp]
  -> (Focus -> m Position)
  -> m [SmtSexp]
traverseZipper roots cps = flip evalStateT initFocus $ do
  zipper
  rebuild <$> get
  where
    initFocus = smtSexprsToFocus roots
    zipper :: StateT Focus m ()
    zipper = do
      focus@(_, hist) <- get
      updatedPos <- lift $ cps focus
      put (updatedPos, hist)

      c <- canMoveDown <$> get
      when c $ do
        modify moveDown
        zipper
        modify moveUp

      d <- canMoveRight <$> get
      when d $ do
        modify moveRight
        zipper

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