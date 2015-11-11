module Infsabot.RobotStrategy(movementOnlyProgram, basicProgram)
    where

import Infsabot.Base
import Infsabot.Constants
import Infsabot.RobotAction

movementOnlyProgram :: Team -> RobotProgram
movementOnlyProgram team state
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
        = case peekAtSpot state [] of
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

basicProgram :: Team -> RobotProgram
basicProgram team state
    | mat == SpotMaterial
        = (Dig, stateMemory state)
    | enemyLoc /= Nothing
        = (Fire {fireDirection = unpack enemyLoc, materialExpended = 2}, stateMemory state)
    | stateAge state `mod` 11 == 0  = directionByMod createSpawn
    | stateAge state `mod` 11 == 1  = directionByMod createSendMessage
    | otherwise = directionByMod MoveIn
    where
    directionByMod f
            = (f direction, stateMemory state)
        where
        modulus = stateAge state `mod` 8
        direction
            = case modulus of
                0 -> N
                1 -> N
                2 -> N
                3 -> E
                4 -> E
                5 -> E
                6 -> W
                7 -> S
    enemyLoc = enemyRobot [N, S, E, W]
        where
        enemyRobot :: [RDirection] -> Maybe RDirection
        enemyRobot [] = Nothing
        enemyRobot (u:us)
            = case peekAtSpot state [u] of
                Just (SeenSpot _ (Just rob))
                    -> if rob /= ourAppearance then Just u else enemyRobot us
                _
                    -> enemyRobot us
    mat
        = case peekAtSpot state [] of
            Nothing -> SpotEmpty
            Just (SeenSpot current _) -> current
    ourAppearance = RobotAppearance {robotColor = colorDefaultOf team}
    createSpawn dir = Spawn {
        newDirection = dir,
        newProgram = basicProgram team,
        newAppearance = ourAppearance,
        newMaterial = material state `div` 3,
        newMemory = stateMemory state
    }
    createSendMessage dir = SendMessage {
        messageToSend = show (stateAge state),
        sendDirection = dir
    }
