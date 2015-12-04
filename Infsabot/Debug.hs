module Infsabot.Debug(printRobot, printAction, printRobotAndAction, displayRAAL, trace) where

import Infsabot.Robot.Interface

import Infsabot.RobotAction.Interface
import qualified Debug.Trace as T

isDEBUG :: Bool
isDEBUG = False

trace :: String -> a -> a
trace msg x
    | isDEBUG       = T.trace msg x
    | otherwise     = x

printRobot :: PositionedRobot -> String
printRobot (PositionedRobot ((x, y), rob)) = show (x, y, robotTeam rob)

printAction :: RobotAction -> String
printAction (Spawn s) = "Spawn " ++ show (newDirection s)
printAction x = show x

printRobotAndAction :: (PositionedRobot, RobotAction) -> String
printRobotAndAction (x, y) = "("++printRobot x ++ ", " ++ printAction y++")"

displayRAAL :: [((PositionedRobot, RobotAction), a)] -> String
displayRAAL raas = show $ map (printRobotAndAction . fst) raas
