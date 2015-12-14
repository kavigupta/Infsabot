{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverlappingInstances #-}

module Infsabot.Strategy.Random.Logic (
        complexity, getDeltas, applyDeltas, constantToParameter, simplify, complicate
    ) where

import Infsabot.RobotAction.Interface
import Infsabot.Strategy.ExprTree.Interface
import Infsabot.Base.Interface

import Data.Graph.Inductive.Query.Monad((><))

import Control.Monad.State.Lazy
import Control.Applicative((<$>))
import Control.Monad(liftM3)

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

instance Random RDirection where
    randomR = const random
    random gen= (case val of 0 -> N; 1 -> E; 2 -> W; _ -> S, gen')
        where
        (val, gen') = randomR (0 :: Int, 3) gen

{-
instance ComplexityRandom ExprDir where
    complexity (ConstDir _) = 1
    complexity (IfDir a b c) = 1 + complexity a + complexity b + complexity c
    cRandom a = runState $ do
        bb <- state random
        if bb && a >= 1 then
            ConstDir <$> state random
        else do
            [a', b', c'] <- state $ getPartition a 3
            liftM3 IfDir (srand a') (srand b') (srand c')

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
    cRandom cxty = runState $ do
        which <- state $ randomR (0, 8)
        case which of
            0 -> ConstBool <$> state random
            1 -> CanSee <$> cRandom
-}

getPartition :: (RandomGen g) => Int -> Int -> g -> ([Int], g)
getPartition n k = value
    where
    value :: (RandomGen g) => g  -> ([Int], g)
    value = choice parts
    parts :: [[Int]]
    parts = map departition $ partitionsWithKParts k n
    departition (Partition x) = x

toList :: ExprPath -> [ExprDir]
toList Here = []
toList (Offset dir rest) = dir:toList rest

fromList :: [ExprDir] -> ExprPath
fromList = foldr Offset Here

choice :: (RandomGen g) => [a] -> g -> (a, g)
choice xs = runState $ do
    index <- state $ randomR (0, length xs - 1)
    return $ xs !! index
