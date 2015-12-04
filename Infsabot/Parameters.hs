{-# Language TemplateHaskell #-}
{-# Language CPP #-}

module Infsabot.Parameters(
        Parameters,
        LinearF(LinearF), apply,
            paramBoardSize, updateBoardSize,
            paramNoopCost, updateNoopCost,
            paramMoveCost, updateMoveCost,
            paramDigCost, updateDigCost,
            paramNewRobotCost, updateNewRobotCost,
            paramFireCost, updateFireCost,
            paramInitialHP, updateInitialHP,
            paramInitialMaterial, updateInitialMaterial,
            paramLineOfSight, updateLineOfSight,
            paramLineOfFire, updateLineOfFire,
            paramLineOfMessageSending, updateLineOfMessageSending,
            paramHPRemoved, updateHPRemoved,
        defaultParameters
    ) where

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck.Arbitrary

data LinearF a = LinearF a a deriving (Eq, Show)

apply :: (Num a) => LinearF a -> a -> a
apply (LinearF m b) x = m * x + b

data Parameters = Parameters {
    _paramBoardSize :: Int,
    _paramNoopCost :: Int,
    _paramMoveCost :: Int,
    _paramDigCost :: Int,
    _paramNewRobotCost :: Int,
    _paramFireCost :: Int,
    _paramInitialHP :: Int,
    _paramInitialMaterial :: Int,
    _paramLineOfSight :: Int,
    _paramLineOfFire :: Int,
    _paramLineOfMessageSending :: Int,
    _paramHPRemoved :: LinearF Int
} deriving (Show)

defaultParameters :: Parameters
defaultParameters = Parameters {
    _paramBoardSize = 75,
    _paramNoopCost = 1,
    _paramMoveCost = 5,
    _paramDigCost = 10,
    _paramNewRobotCost = 20,
    _paramFireCost = 5,
    _paramInitialHP = 100,
    _paramInitialMaterial = 50,
    _paramLineOfSight = 5,
    _paramLineOfFire = 3,
    _paramLineOfMessageSending = 4,
    _paramHPRemoved = LinearF 1 2
}

updateHPRemoved :: LinearF Int -> Parameters -> Parameters
updateHPRemoved (LinearF m b) params = params {_paramHPRemoved = (LinearF (max m 0) (max b 0))}

paramHPRemoved :: Parameters -> LinearF Int
paramHPRemoved = _paramHPRemoved

#define PARAM_AND_UPDATE(UPDATER, ACCESSOR, INTERNAL) UPDATER :: Int -> Parameters -> Parameters; UPDATER n params = params {INTERNAL = max n 0}; ACCESSOR :: Parameters -> Int; ACCESSOR = INTERNAL

PARAM_AND_UPDATE(updateBoardSize, paramBoardSize, _paramBoardSize)
PARAM_AND_UPDATE(updateNoopCost, paramNoopCost, _paramNoopCost)
PARAM_AND_UPDATE(updateMoveCost, paramMoveCost, _paramMoveCost)
PARAM_AND_UPDATE(updateDigCost, paramDigCost, _paramDigCost)
PARAM_AND_UPDATE(updateNewRobotCost, paramNewRobotCost, _paramNewRobotCost)
PARAM_AND_UPDATE(updateFireCost, paramFireCost, _paramFireCost)
PARAM_AND_UPDATE(updateInitialHP, paramInitialHP, _paramInitialHP)
PARAM_AND_UPDATE(updateInitialMaterial, paramInitialMaterial, _paramInitialMaterial)
PARAM_AND_UPDATE(updateLineOfSight, paramLineOfSight, _paramLineOfSight)
PARAM_AND_UPDATE(updateLineOfFire, paramLineOfFire, _paramLineOfFire)
PARAM_AND_UPDATE(updateLineOfMessageSending, paramLineOfMessageSending, _paramLineOfMessageSending)

$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )
