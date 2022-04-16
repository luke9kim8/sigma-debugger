{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SigmaM where

import qualified Control.Monad.Trans.State as TS
import Control.Monad.State
import System.Random
import Control.Monad.IO.Class
import Debug.Trace

type SigmaState s = (s, StdGen)
newtype SigmaM s a = SigmaM {unSigmaM :: TS.StateT (SigmaState s) IO a}

instance Functor (SigmaM s) where
  fmap f (SigmaM k) = SigmaM (fmap f k)

instance Applicative (SigmaM s) where
  pure k = SigmaM (pure k)
  (SigmaM f) <*> (SigmaM k) = SigmaM (f <*> k)

instance Monad (SigmaM s) where
  (>>=) (SigmaM k) f = SigmaM (k >>= unSigmaM . f)

instance MonadState s (SigmaM s) where
  get = SigmaM $ fst <$> get
  put n = SigmaM $ do
    (_, r) <- get
    put (n, r)

instance MonadIO (SigmaM s) where
  liftIO = SigmaM . liftIO

class Monad m => SigmaRandom m where
  randomBool :: m Bool
  performRandomly :: a -> a -> m a
  performRandomly q1 q2 = do
    b <- randomBool
    return $ if b then q1 else q2

instance SigmaRandom (SigmaM s) where
  randomBool = SigmaM $ do
    (s, g) <- get
    let (b, g') = random g
    put (s, g')
    return $ trace (show b) b

runSigmaM :: SigmaM s a -> s -> Int -> IO (a, s)
runSigmaM (SigmaM sm) s seed = do 
  (a, (s, _)) <- runStateT sm (s, mkStdGen seed)
  return (a, s)

evalSigmaM :: SigmaM s a -> s -> Int -> IO a
evalSigmaM (SigmaM sm) s seed = do 
  (a, (_, _)) <- runStateT sm (s, mkStdGen seed)
  return a