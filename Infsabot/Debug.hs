module Infsabot.Debug(printRobot, printAction, printRobotAndAction) where

import Infsabot.Robot
import Infsabot.RobotAction

printRobot :: (Int, Int, Robot) -> String
printRobot (x, y, rob) = show (x, y, robotTeam rob)

printAction :: RobotAction -> String
printAction s@(Spawn _ _ _ _ _) = "Spawn " ++ show (newDirection s)
printAction x = show x

printRobotAndAction :: ((Int, Int, Robot), RobotAction) -> String
printRobotAndAction (x, y) = "["++printRobot x ++ ", " ++ printAction y++"]"
