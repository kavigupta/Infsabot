module Infsabot.Robot (
        Robot(Robot),
            robotProgram,
            robotTeam,
            robotAppearance,
            robotMaterial,
            robotBirthdate,
            robotMemory,
            robotHitpoints,
            robotMessages,
        defaultRobot,
        RobotAndAction,
        PositionedRobot(PositionedRobot),
            getLocation
    ) where

import Infsabot.Base
import Infsabot.RobotAction
import qualified Data.Map as M
import Infsabot.Parameters

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
		robotMemory :: InternalState,
        -- The messages this robot has on the stack
        robotMessages :: [(String, RDirection)]
} deriving (Eq, Show)

-- A robot associated with a given position
newtype PositionedRobot = PositionedRobot ((Int, Int), Robot) deriving (Eq, Show)
-- A robot associated with a given action
type RobotAndAction = (PositionedRobot, RobotAction)

getLocation :: PositionedRobot -> (Int, Int)
getLocation (PositionedRobot (xy, _)) = xy

defaultRobot :: Parameters -> Team -> RobotProgram -> Robot
defaultRobot p team program = Robot {
    robotProgram = program,
    robotTeam = team,
    robotAppearance = RobotAppearance $ colorOf team,
    robotMaterial = paramInitialMaterial p,
    robotHitpoints = paramInitialHP p,
    robotBirthdate = 0,
    robotMemory = M.empty,
    robotMessages = []
}
