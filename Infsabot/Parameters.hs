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
            paramLineOfSight,
            paramLineOfFire,
            paramLineOfMessageSending,
            paramHPRemoved,
        defaultParameters
    ) where

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck.Arbitrary
import Infsabot.Tools.Interface

data LinearF a = LinearF a a deriving (Eq, Show)

apply :: (Num a) => LinearF a -> a -> a
apply (LinearF m b) x = m * x + b

data Parameters = Parameters {
    paramBoardSize :: Natural,
    paramNoopCost :: Natural,
    paramMoveCost :: Natural,
    paramDigCost :: Natural,
    paramNewRobotCost :: Natural,
    paramFireCost :: Natural,
    paramInitialHP :: Natural,
    paramInitialMaterial :: Natural,
    paramLineOfSight :: Natural,
    paramLineOfFire :: Natural,
    paramLineOfMessageSending :: Natural,
    paramHPRemoved :: LinearF Natural
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
    paramLineOfSight = 5,
    paramLineOfFire = 3,
    paramLineOfMessageSending = 4,
    paramHPRemoved = LinearF 1 2
}

$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )
