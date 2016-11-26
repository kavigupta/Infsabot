module Infsabot.Robot.Logic (
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

import Infsabot.Base.Interface
import Infsabot.RobotAction.Interface
import qualified Data.Map as M
import Infsabot.Parameters
import Infsabot.Tools.Interface

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
    robotMaterial = unNatural $ paramInitialMaterial p,
    robotHitpoints = unNatural $ paramInitialHP p,
    robotBirthdate = 0,
    robotMemory = InternalState M.empty,
    robotMessages = []
}
