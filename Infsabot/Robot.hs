module Robot() where

import Infsabot.Board
import qualified Data.Map as M

-- Represents one of the 4 potential directions 
data Direction = N | E | W | S

-- Represents a Team. Currently, there are only two teams.
data Team = A | B

-- Represents a Spot on the Board as seen by a robot. 
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data SeenSpot = SeenSpot {boardSpot :: BoardSpot, robotAt :: Maybe RobotAppearance}

-- Represents an offset from the original position.
newtype Offset = Offset Int

-- The robot's internal state. This is represented by a Stringly-typed Map
type InternalState = M.Map String String

-- The Robot's concept of self.
data KnownState = KnownState {
	-- Function the Robot can use to see around it. Returns nothing if the robot can't see that far.
	peekAtSpot :: Offset -> Offset -> Maybe SeenSpot,
	-- The material the robot is standing on
	material :: Int, 
	-- The current location of the robot
	stateLocation :: (Int, Int),
	-- The current time, since robot's "birth"
	stateTime :: Int,
	-- The robot's memory
	stateMemory :: InternalState
}

type RobotProgram = KnownState -> (RobotAction, InternalState)

data RobotAction = MoveIn Direction | Dig | Spawn {
	newProgram :: RobotProgram, 
	newAppearance :: RobotAppearance,
	newMaterial :: Int, 
	newMemory :: InternalState
}

data Robot = Robot {
		robotProgram :: RobotProgram,
		robotTeam :: Team,
		robotAppearance :: RobotAppearance,
		robotMaterial :: Int,
		robotState :: InternalState
	}

data RobotAppearance = RobotAppearance {
	robotColor :: (Int, Int, Int)
}
