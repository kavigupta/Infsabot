module Infsabot.GamePlay() where

import Infsabot.Board
import Infsabot.Robot

getKnownState :: Board -> (Int, Int, Robot) -> KnownState
getKnownState b (x, y, rob) = KnownState {
		peekAtSpot = peekFn,
		material = robotMaterial rob,
		stateLocation = (x,y),
		stateAge = boardTime b - robotBirthdate rob,
		stateMemory = robotMemory rob
	}
	where
	peekFn :: (Offset, Offset) -> Maybe SeenSpot
	peekFn (Offset x) (Offset y)
		| x * x + y * y <= lineOfSight robot ^ 2	= Just $ asSeenSpot $ getKnownState b
		| True										= Nothing
