module Infsabot.Parameters(
        Parameters(Parameters),
            paramNoopCost,
            paramMoveCost,
            paramDigCost,
            paramNewRobotCost,
            paramFireCost,
            paramInitialHP,
            paramInitialMaterial,
            lineOfSight,
            lineOfFire,
            lineOfMessageSending
    ) where

data Parameters = Parameters {
    paramNoopCost :: Int,
    paramMoveCost :: Int,
    paramDigCost :: Int,
    paramNewRobotCost :: Int,
    paramFireCost :: Int,
    paramInitialHP :: Int,
    paramInitialMaterial :: Int,
    lineOfSight :: Int,
    lineOfFire :: Int,
    lineOfMessageSending :: Int
}
