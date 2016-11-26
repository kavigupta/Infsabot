{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Infsabot.RobotAction.Logic (
        RobotProgram, RobotProgramResult,
        KnownState(KnownState),
            peekAtSpot, material, stateLocation, stateAge, stateMemory, stateMessages,
        RobotAction(Die, Noop, MoveIn, Dig, Spawn, Fire, Send),
            SpawnAction(SpawnAction), SendAction(SendAction), FireAction(FireAction),
            newProgram, newAppearance, newMaterial, newMemory, newDirection,
            fireDirection, materialExpended,
            messageToSend, sendDirection,
        orderOfOperations,
        actionCost
    ) where

import Infsabot.Base.Interface
import Infsabot.Parameters
import Infsabot.Tools.Interface

-- A robot program takes the Robot's state and returns a RobotProgramResult
type RobotProgram = KnownState -> RobotProgramResult

-- A robot program result consists of an action and a potentially modified internal state
type RobotProgramResult = (RobotAction, InternalState)

-- The Robot's concept of self.
data KnownState = KnownState {
    -- Function the Robot can use to see around it.
    -- Returns Nothing if the robot can't see that far.
    peekAtSpot :: [RDirection] -> Maybe SeenSpot,
    -- The quantity of material the robot has
    material :: Int,
    -- The current location of the robot
    stateLocation :: (Int, Int),
    -- The robot's age
    stateAge :: Int,
    -- The robot's memory
    stateMemory :: InternalState,
    -- The robot's received messages as a list of pairs of
    -- message and direction received.
    stateMessages :: [(String, RDirection)]
} deriving Show

-- Represents an action a robot can take.
-- If the action is impossible, nothing will occur
data RobotAction =
                -- Robot will die
                Die |
                -- Robot will do nothing
                Noop |
                -- Robot will fire in a given direction
                Fire FireAction |
                -- Robot will send a message in a given direction
                Send SendAction |
                -- Robot will dig
                Dig |
                -- Robot will move in the given Direction
                MoveIn RDirection |
                -- Robot will spawn a new Robot
                Spawn SpawnAction
    deriving (Show, Eq)

data FireAction = FireAction {
    -- Material devoted to this task.
    -- More material means greater blow
    materialExpended :: Natural,
    -- Direction to fire in
    fireDirection :: RDirection
} deriving (Show, Eq)

data SendAction = SendAction {
    -- The message to send to another robot
    messageToSend :: String,
    -- The direction to send the message in
    sendDirection :: RDirection
} deriving (Show, Eq)

data SpawnAction = SpawnAction {
    -- The direction the new robot will be placed in
    newDirection :: RDirection,
    -- The program the new Robot will have
    newProgram :: RobotProgram,
    -- The appearance of the new Robot
    newAppearance :: RobotAppearance,
    -- The quantity of material to transfer to the new robot
    newMaterial :: Natural,
    -- The memory of the new robot
    newMemory :: InternalState
} deriving (Show, Eq)

instance Show ([RDirection] -> Maybe SeenSpot) where
    show _ = "Classified"

instance Show RobotProgram where
    show _ = "basicProgram A"

instance Eq RobotProgram where
    _ == _ = True


orderOfOperations :: RobotAction -> Int
orderOfOperations Die = 0
orderOfOperations Noop = 1
orderOfOperations (Fire _) = 2
orderOfOperations (Send _) = 3
orderOfOperations Dig = 4
orderOfOperations (MoveIn _) = 5
orderOfOperations (Spawn _) = 6

-- Outputs the cost of performing the given action.
actionCost :: Parameters -> RobotAction -> Natural
actionCost p Noop = paramNoopCost p
actionCost _ Die = 0
actionCost p (MoveIn _) = paramMoveCost p
actionCost p Dig = paramDigCost p
actionCost p (Spawn s) = newMaterial s + paramNewRobotCost p
actionCost p (Fire f) = materialExpended f + paramFireCost p
actionCost p (Send _) = actionCost p Noop
