{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverlappingInstances #-}
module Infsabot.Strategy.Random.Logic (
        complexity, getDeltas, applyDeltas, constantToParameter, simplify, complicate
    ) where

import Infsabot.RobotAction.Interface
import Infsabot.Strategy.ExprTree.Interface

import Data.Graph.Inductive.Query.Monad((><))

import Control.Monad.State.Lazy
import Control.Applicative((<$>))

import Data.Ratio

import System.Random
import Math.Combinat.Partitions.Integer

type HistoricalStates = [KnownState]


class ComplexityRandom a where
    complexity :: a -> Int
    cRandom :: (RandomGen g) => Int -> g -> (a, g)

instance (ComplexityRandom a) => Random a where
    randomR (a, b) = runState $ do
            c <- state $ randomR (ca, cb)
            state $ cRandom c
        where
        ca = complexity a
        cb = complexity b
    random = runState $ do
        val <- state $ randomR (1, 1000)
        state $ cRandom val

class (Random a) => Expr a where
    getDeltas :: Ratio Int -> a -> [a]
    applyDeltas :: [Ratio Int] -> a -> a
    constantToParameter :: HistoricalStates -> a -> StdGen -> (a, StdGen)
    simplify :: HistoricalStates -> a -> StdGen -> (a, StdGen)
    complicate :: Int -> a -> StdGen -> (a, StdGen)

srand :: (ComplexityRandom a, RandomGen g) => Int -> State g a
srand = state . cRandom

instance ComplexityRandom ExprDir where
    complexity (ConstDir _) = 1
    complexity (IfDir a b c) = 1 + complexity a + complexity b + complexity c
    cRandom a = runState $ do
        bb <- state $ random
        if bb && a >= 1 then
            ConstDir <$> state random
        else
            liftM3 IfDir (srand (a, b)) (srand (a,b)) (srand (a,b))

instance ComplexityRandom ExprBool where
    complexity (ConstBool _) = 1
    complexity (CanSee x) = 1 + complexity x
    complexity (MaterialAt x) = 1 + complexity x
    complexity (RobotAt x) = 1 + complexity x
    complexity (EqualInt x y) = 1 + complexity x + complexity y
    complexity (GreaterInt x y) = 1 + complexity x + complexity y
    complexity (EqualDir x y) = 1 + complexity x + complexity y
    complexity (x :& y) = 1 + complexity x + complexity y
    complexity (x :| y) = 1 + complexity x + complexity y
    complexity (Not x) = 1 + complexity x
    cRandom (a, b) = undefined

getPartition :: (Random g) => Int -> Int -> State g [Int]
getPartition n k = state $ runRVarT (choice parts)
    where
    parts = partitionsWithKParts k n
toList :: ExprPath -> [ExprDir]
toList Here = []
toList (Offset dir rest) = dir:toList rest

fromList :: [ExprDir] -> ExprPath
fromList [] = Here
fromList (x:xs) = Offset x (fromList xs)
