module Infsabot.Parameters(
        Parameters(Parameters),
            boardSize,
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

data Parameters = Parameters {
    boardSize :: Int,
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
    hitpointsRemoved :: Int -> Int
}

defaultParameters :: Parameters
defaultParameters = Parameters {
    boardSize = 500,
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
    hitpointsRemoved = \x -> x + 2
}
