{-# Language TemplateHaskell #-}
{-# Language CPP #-}

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
    paramBoardSize = makeNatural 75,
    paramNoopCost = makeNatural 1,
    paramMoveCost = makeNatural 5,
    paramDigCost = makeNatural 10,
    paramNewRobotCost = makeNatural 20,
    paramFireCost = makeNatural 5,
    paramInitialHP = makeNatural 100,
    paramInitialMaterial = makeNatural 50,
    paramLineOfSight = makeNatural 5,
    paramLineOfFire = makeNatural 3,
    paramLineOfMessageSending = makeNatural 4,
    paramHPRemoved = LinearF (makeNatural 1) (makeNatural 2)
}

$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )
