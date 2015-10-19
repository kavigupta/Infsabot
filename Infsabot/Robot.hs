module Infsabot.Robot(Robot(Robot), SeenSpot(SeenSpot), GameSpot(GameSpot), toSeenSpot, robotAppearance, robotColor) where

import Codec.Picture (PixelRGB8)
import qualified Data.Map as M
import Infsabot.Base

-- Represents one of the 4 potential directions
data Direction = N | E | W | S

-- Represents a Team. Currently, there are only two teams.
data Team = A | B

-- Represents an offset from the original position.
newtype Offset = Offset Int

-- The robot's internal state. This is represented by a Stringly-typed Map
type InternalState = M.Map String String

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data SeenSpot = SeenSpot BoardSpot (Maybe RobotAppearance)

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data GameSpot = GameSpot BoardSpot (Maybe Robot)

toSeenSpot :: GameSpot -> SeenSpot
toSeenSpot (GameSpot s Nothing) = SeenSpot s Nothing
toSeenSpot (GameSpot s (Just rob)) = SeenSpot s $ Just $ robotAppearance rob

-- The Robot's concept of self.
data KnownState = KnownState {
	-- Function the Robot can use to see around it. Returns nothing if the robot can't see that far.
	peekAtSpot :: Offset -> Offset -> Maybe SeenSpot,
	-- The material the robot is standing on
	material :: Int,
	-- The current location of the robot
	stateLocation :: (Int, Int),
	-- The robot's age
	stateAge :: Int,
	-- The robot's memory
	stateMemory :: InternalState
}

type RobotProgram = KnownState -> (RobotAction, InternalState)

data RobotAction = MoveIn Direction
                 | Dig
                 | Spawn {
                	newProgram :: RobotProgram,
                	newAppearance :: RobotAppearance,
                	newMaterial :: Int,
                	newMemory :: InternalState
                }
                | Fire {
                    fireDirection :: Direction,
                    materialExpended :: Int
                }

data Robot = Robot {
		robotProgram :: RobotProgram,
		robotTeam :: Team,
		robotAppearance :: RobotAppearance,
		robotMaterial :: Int,
        robotBirthdate :: Int,
		robotMemory :: InternalState
}

lineOfSight :: Robot -> Int
lineOfSight _ = 1

data RobotAppearance = RobotAppearance {
	robotColor :: PixelRGB8
}
