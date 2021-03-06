module Infsabot.Strategy.BasicStrategy(basicProgram2, basicProgram)
    where


import Data.Maybe

import Infsabot.Base.Interface
import Infsabot.RobotAction.Interface
import Infsabot.Tools.Interface

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
        = case peekAtSpot state [] of
            Nothing -> SpotEmpty
            Just (SeenSpot current _) -> current
    createSpawn dir = Spawn SpawnAction {
        newDirection = dir,
        newProgram = basicProgram team,
        newAppearance = RobotAppearance $ colorOf team,
        newMaterial = makeNatural $ material state `div` 3,
        newMemory = stateMemory state
    }

basicProgram2 :: Team -> RobotProgram
basicProgram2 asdf state
    | mat == SpotMaterial
        = (Dig, stateMemory state)
    | isJust enemyLoc
        = (Fire FireAction {fireDirection = fromJust enemyLoc, materialExpended = 2}, stateMemory state)
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
                1 -> E
                2 -> N
                3 -> E
                4 -> E
                5 -> E
                6 -> W
                _ -> S
    enemyLoc = head <$> enemyRobot (concatMap duplicator [N, S, E, W])
        where
        duplicator :: RDirection -> [[RDirection]]
        duplicator x = [[x], [x,x]]
        enemyRobot :: [[RDirection]] -> Maybe [RDirection]
        enemyRobot [] = Nothing
        enemyRobot (u:us)
            = case peekAtSpot state u of
                Just (SeenSpot _ (Just rob))
                    -> if rob /= ourAppearance then Just u else enemyRobot us
                _
                    -> enemyRobot us
    mat
        = case peekAtSpot state [] of
            Nothing -> SpotEmpty
            Just (SeenSpot current _) -> current
    ourAppearance = RobotAppearance $ colorOf asdf
    createSpawn dir = Spawn SpawnAction{
        newDirection = dir,
        newProgram = basicProgram asdf,
        newAppearance = ourAppearance,
        newMaterial = makeNatural $ material state `div` 3,
        newMemory = stateMemory state
    }
    createSendMessage dir = Send SendAction {
        messageToSend = show (stateAge state),
        sendDirection = dir
    }
