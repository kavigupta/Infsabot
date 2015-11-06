{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Infsabot.RobotAction (
        RobotProgram, RobotProgramResult,
        KnownState(KnownState),
            peekAtSpot, material, stateLocation, stateAge, stateMemory, stateMessages,
        RobotAction(Die, Noop, MoveIn, Dig, Spawn, Fire, SendMessage),
            newProgram, newAppearance, newMaterial, newMemory, newDirection,
            fireDirection, materialExpended,
            messageToSend, sendDirection,
            orderOfOperations,
        actionCost
    ) where

import Infsabot.Base
import Infsabot.Parameters

-- A robot program takes the Robot's state and returns a RobotProgramResult
type RobotProgram = KnownState -> RobotProgramResult

instance Show (RobotProgram) where
    show _ = "Program"

instance Eq (RobotProgram) where
    _ == _ = True

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
}

-- Represents an action a robot can take.
-- If the action is impossible, nothing will occur
data RobotAction =
                -- Robot will die
                Die |
                -- Robot will do nothing
                Noop |
                -- Robot will fire in a given direction
                Fire {
                    -- Direction to fire in
                    fireDirection :: RDirection,
                    -- Material devoted to this task.
                    -- More material means greater blow
                    materialExpended :: Int
                } |
                -- Robot will send a message in a given direction
                SendMessage {
                    -- The message to send to another robot
                    messageToSend :: String,
                    -- The direction to send the message in
                    sendDirection :: RDirection
                } |
                -- Robot will dig
                Dig |
                -- Robot will move in the given Direction
                MoveIn RDirection |
                -- Robot will spawn a new Robot
                Spawn {
                    -- The direction the new robot will be placed in
                    newDirection :: RDirection,
                    -- The program the new Robot will have
                    newProgram :: RobotProgram,
                    -- The appearance of the new Robot
                	newAppearance :: RobotAppearance,
                    -- The quantity of material to transfer to the new robot
                	newMaterial :: Int,
                    -- The memory of the new robot
                	newMemory :: InternalState
                }
    deriving (Show, Eq)

orderOfOperations :: RobotAction -> Int
orderOfOperations Die = 0
orderOfOperations Noop = 1
orderOfOperations (Fire _ _) = 2
orderOfOperations (SendMessage _ _) = 3
orderOfOperations Dig = 4
orderOfOperations (MoveIn _) = 5
orderOfOperations (Spawn _ _ _ _ _) = 6

-- Outputs the cost of performing the given action.
actionCost :: Parameters -> RobotAction -> Int
actionCost p Noop = paramNoopCost p
actionCost _ Die = 0
actionCost p (MoveIn _) = paramMoveCost p
actionCost p Dig = paramDigCost p
actionCost p s@(Spawn _ _ _ _ _) = newMaterial s + paramNewRobotCost p
actionCost p f@(Fire _ _) = materialExpended f + paramFireCost p
actionCost p (SendMessage _ _) = actionCost p Noop
