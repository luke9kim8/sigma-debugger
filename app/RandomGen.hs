module RandomGen where

import System.Random (StdGen, mkStdGen, next)
-- import State
import Control.Monad 

testRandom :: Int -> Int
testRandom i = fst (next (mkStdGen i))