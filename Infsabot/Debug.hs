module Infsabot.Debug(printRobot, printAction, printRobotAndAction, displayRAAL, trace) where

import Infsabot.Robot
import Infsabot.RobotAction
import qualified Debug.Trace as T

isDEBUG :: Bool
isDEBUG = False


trace :: String -> a -> a
trace msg x
    | isDEBUG       = T.trace msg x
    | otherwise     = x

printRobot :: (Int, Int, Robot) -> String
printRobot (x, y, rob) = show (x, y, robotTeam rob)

printAction :: RobotAction -> String
printAction s@(Spawn _ _ _ _ _) = "Spawn " ++ show (newDirection s)
printAction x = show x

printRobotAndAction :: ((Int, Int, Robot), RobotAction) -> String
printRobotAndAction (x, y) = "("++printRobot x ++ ", " ++ printAction y++")"

displayRAAL :: [(((Int, Int, Robot), RobotAction), a)] -> String
displayRAAL raas = show $ map (printRobotAndAction . fst) raas
