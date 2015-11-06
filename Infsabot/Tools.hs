module Infsabot.Tools (shuffle) where

import System.Random
import Data.Array.ST hiding (newArray)
import Control.Monad
import Control.Monad.ST
import Data.STRef

shuffle :: Int -> [a] -> [a]
shuffle seed lst = fst $ stdLibShuffle lst $ mkStdGen seed

--- from https://wiki.haskell.org/Random_shuffle.
-- No idea why the function from random-shuffle library is not working
stdLibShuffle :: [a] -> StdGen -> ([a],StdGen)
stdLibShuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray u =  newListArray (1,u)
