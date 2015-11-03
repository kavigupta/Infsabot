module Infsabot.RobotStrategy(basicProgram)
    where

import Infsabot.Base
import Infsabot.Constants
import Infsabot.RobotAction

basicProgram :: Team -> RobotProgram
basicProgram team state
    | mat == SpotMaterial
        = (Dig, stateMemory state)
    | stateAge state `mod` 9 /= 0
        = if stateAge state `mod` 2 == 0 then
            (MoveIn N, stateMemory state) else
            (MoveIn E, stateMemory state)
    | otherwise
        = if stateAge state `mod` 2 == 0 then
            (createSpawn N, stateMemory state) else
            (createSpawn E, stateMemory state)
    where
    mat
        = case peekAtSpot state (Offset 0, Offset 0) of
            Nothing -> SpotEmpty
            Just (SeenSpot current _) -> current
    createSpawn dir = Spawn {
        newDirection = dir,
        newProgram = basicProgram team,
        newAppearance = RobotAppearance {
            robotColor = colorDefaultOf team
        },
        newMaterial = material state `div` 3,
        newMemory = stateMemory state
    }
