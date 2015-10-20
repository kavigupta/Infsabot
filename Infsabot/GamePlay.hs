module Infsabot.GamePlay() where

import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Base

-- The distance the given robot can see
lineOfSight :: Robot -> Int
lineOfSight _ = 1

-- The distance the robot can fire
lineOfFire :: Robot -> Int
lineOfFire _ = 2

-- Increments time by one
applyTimeTick :: Board -> Board
applyTimeTick b = b {boardTime = boardTime b + 1}

getRobotActions :: Board -> [((Int, Int, Robot), RobotProgramResult)]
getRobotActions b = map (getRobotAction b) $ boardRobots b
	where
	getRobotAction b (x, y, rob) = ((x, y, rob), robotProgram rob state)
	   where state = getKnownState b (x, y, rob)

getKnownState :: Board -> (Int, Int, Robot) -> KnownState
getKnownState b (x, y, rob) = KnownState {
		peekAtSpot = peekFn,
		material = robotMaterial rob,
		stateLocation = (x,y),
		stateAge = boardTime b - robotBirthdate rob,
		stateMemory = robotMemory rob
	}
	where
	peekFn :: Offset ->  Offset -> Maybe SeenSpot
	peekFn (Offset offx) (Offset offy)
		| withinRange	= Just $ toSeenSpot $ b !!! (x + offx, y + offy)
		| otherwise		= Nothing
            where withinRange = offx * offx + offy * offy <= lineOfSight rob ^ 2
