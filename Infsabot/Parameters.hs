{-# Language TemplateHaskell #-}

module Infsabot.Parameters(
        LinearF(LinearF), apply,
        Parameters(Parameters),
            paramBoardSize,
            paramNoopCost,
            paramMoveCost,
            paramDigCost,
            paramNewRobotCost,
            paramFireCost,
            paramInitialHP,
            paramInitialMaterial,
            lineOfSight,
            lineOfFire,
            lineOfMessageSending,
            hitpointsRemoved,
        defaultParameters
    ) where

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck.Arbitrary

data LinearF a = LinearF a a deriving (Eq, Show)

apply :: (Num a) => LinearF a -> a -> a
apply (LinearF m b) x = m * x + b

data Parameters = Parameters {
    paramBoardSize :: Int,
    paramNoopCost :: Int,
    paramMoveCost :: Int,
    paramDigCost :: Int,
    paramNewRobotCost :: Int,
    paramFireCost :: Int,
    paramInitialHP :: Int,
    paramInitialMaterial :: Int,
    lineOfSight :: Int,
    lineOfFire :: Int,
    lineOfMessageSending :: Int,
    hitpointsRemoved :: LinearF Int
} deriving (Show)

defaultParameters :: Parameters
defaultParameters = Parameters {
    paramBoardSize = 75,
    paramNoopCost = 1,
    paramMoveCost = 5,
    paramDigCost = 10,
    paramNewRobotCost = 20,
    paramFireCost = 5,
    paramInitialHP = 100,
    paramInitialMaterial = 50,
    lineOfSight = 5,
    lineOfFire = 3,
    lineOfMessageSending = 4,
    hitpointsRemoved = LinearF 1 2
}

$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )
