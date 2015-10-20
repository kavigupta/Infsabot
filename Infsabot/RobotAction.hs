module Infsabot.RobotAction (
        RobotProgram, RobotProgramResult,
        KnownState(KnownState),
            peekAtSpot, material, stateLocation, stateAge, stateMemory,
        RobotAction(MoveIn, Dig, Spawn, Fire),
            newProgram, newAppearance, newMaterial, newMemory,
            fireDirection, materialExpended
    ) where

import Infsabot.Base

-- A robot program takes the Robot's state and returns a RobotProgramResult
type RobotProgram = KnownState -> RobotProgramResult

-- A robot program result consists of an action and a potentially modified internal state
type RobotProgramResult = (RobotAction, InternalState)

-- The Robot's concept of self.
data KnownState = KnownState {
    -- Function the Robot can use to see around it.
    -- Returns Nothing if the robot can't see that far.
	peekAtSpot :: Offset -> Offset -> Maybe SeenSpot,
	-- The quantity of material the robot has
	material :: Int,
	-- The current location of the robot
	stateLocation :: (Int, Int),
	-- The robot's age
	stateAge :: Int,
	-- The robot's memory
	stateMemory :: InternalState
}

-- Represents an action a robot can take.
-- If the action is impossible, nothing will occur
data RobotAction =
                -- Robot will move in the given Direction
                MoveIn Direction |
                -- Robot will dig
                Dig |
                -- Robot will spawn a new Robot
                Spawn {
                    -- The program the new Robot will have
                    newProgram :: RobotProgram,
                    -- The appearance of the new Robot
                	newAppearance :: RobotAppearance,
                    -- The quantity of material to transfer to the new robot
                	newMaterial :: Int,
                    -- The memory of the new robot
                	newMemory :: InternalState
                } |
                -- Robot will fire in a given direction
                Fire {
                    -- Direction to fire in
                    fireDirection :: Direction,
                    -- Material devoted to this task.
                    -- More material means greater blow
                    materialExpended :: Int
                }