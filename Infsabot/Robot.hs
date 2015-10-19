module Infsabot.Robot (
        Robot(Robot), robotProgram,	robotTeam, robotAppearance, robotMaterial, robotBirthdate, robotMemory,
        SeenSpot(SeenSpot), GameSpot(GameSpot), toSeenSpot,
        robotColor, lineOfSight,
        KnownState(KnownState), peekAtSpot, material, stateLocation, stateAge, stateMemory
    ) where

import Codec.Picture (PixelRGB8)
import Infsabot.Base

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
