module Infsabot.Robot (
        Robot(Robot),
            robotProgram,
            robotTeam,
            robotAppearance,
            robotMaterial,
            robotBirthdate,
            robotMemory,
        GameSpot(GameSpot), toSeenSpot
    ) where

import Infsabot.Constants
import Infsabot.Base
import Infsabot.RobotAction

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data GameSpot = GameSpot BoardSpot (Maybe Robot)

-- Converts a GameSpot to a seen spot
toSeenSpot :: GameSpot -> SeenSpot
toSeenSpot (GameSpot s Nothing) = SeenSpot s Nothing
toSeenSpot (GameSpot s (Just rob)) = SeenSpot s $ Just $ robotAppearance rob

-- Represents a game piece, called a Robot
data Robot = Robot {
        -- The program this Robot will use
		robotProgram :: RobotProgram,
        -- The team this robot belongs to
		robotTeam :: Team,
        -- How this robot appears to its surroundings
		robotAppearance :: RobotAppearance,
        -- The quantity of material this robot contains
		robotMaterial :: Int,
        -- The hitpoints of this robot
        robotHitpoints :: Int,
        -- The birthdate of this robot
        robotBirthdate :: Int,
        -- The memory of this robot
		robotMemory :: InternalState
}
