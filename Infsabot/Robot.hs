module Infsabot.Robot (
        Robot(Robot), robotProgram,	robotTeam, robotAppearance, robotMaterial, robotBirthdate, robotMemory,
        GameSpot(GameSpot), toSeenSpot,
        lineOfSight,
    ) where

import Infsabot.Base
import Infsabot.RobotAction

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data GameSpot = GameSpot BoardSpot (Maybe Robot)

toSeenSpot :: GameSpot -> SeenSpot
toSeenSpot (GameSpot s Nothing) = SeenSpot s Nothing
toSeenSpot (GameSpot s (Just rob)) = SeenSpot s $ Just $ robotAppearance rob

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
