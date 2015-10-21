module Infsabot.Parameters(
        Parameters(Parameters),
            paramNoopCost,
            paramMoveCost,
            paramDigCost,
            paramNewRobotCost,
            paramFireCost
    ) where

data Parameters = Parameters {
    paramNoopCost :: Int,
    paramMoveCost :: Int,
    paramDigCost :: Int,
    paramNewRobotCost :: Int,
    paramFireCost :: Int
}
