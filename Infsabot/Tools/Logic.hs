module Infsabot.Tools.Logic (
        isPrime,
        Natural, makeNatural, unNatural,
        shuffle, allDifferent, sameElements, (!-!), spanNeq,
        doChecks, (~~>)
    ) where

import System.Random
import Data.Array.ST hiding (newArray)
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List(sort, (\\))
import Test.QuickCheck hiding (shuffle)

isPrime :: Int -> Bool
isPrime x
        | x < 2         =  False
        | otherwise     = not . or $ map (\f -> x `mod` f == 0) [2..maxFactor]
        where
        sqrtX :: Double
        sqrtX = sqrt $ fromIntegral x
        maxFactor :: Int
        maxFactor = floor sqrtX

newtype Natural = Natural Int deriving Eq

makeNatural :: Int -> Natural
makeNatural = Natural . max 0

unNatural :: Natural -> Int
unNatural (Natural x) = x

instance Show Natural where
    show = show . unNatural

spanNeq :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
spanNeq _ _ [] = ([], [])
spanNeq f filt (x:xs)
    | f x
        = if not (filt x)
            then span f xs
            else
                let (a, b) = spanNeq f filt xs
                in (x:a, b)
    | otherwise
        = ([], x:xs)

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements a b = length a == length b && (null $ a \\ b)

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

allDifferent :: (Ord a) => [a] -> Bool
allDifferent us = allDiffSorted (sort us)
    where
    allDiffSorted [] = True
    allDiffSorted [_] = True
    allDiffSorted (x:y:xs)
        | x == y    = False
        | otherwise = allDiffSorted (y:xs)

(!-!) :: [[a]] -> Int -> [a]
[] !-! _ = []
(x:_) !-! 0 = x
(_:xs) !-! n = xs !-! (n - 1)

(~~>) :: Bool -> Bool -> Bool
a ~~> b = not a || b

checkCount :: Int
checkCount = 1000

doChecks :: (Test.QuickCheck.Testable prop) => Int -> prop -> IO Result
doChecks n = quickCheckWithResult $ stdArgs { maxSuccess = n * checkCount }
