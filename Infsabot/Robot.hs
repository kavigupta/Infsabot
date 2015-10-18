module Robot() where

import Infsabot.Board

data Direction = N | E | W | S

data Team = A | B

data Spot = Spot {boardSpot :: BoardSpot, robotAt :: Maybe Robot}

newtype Offset = Offset Int

data KnownState = KnownState {
		vision :: Offset -> Offset -> Maybe Spot, 
		material :: Int, 
		location :: (Int, Int), 
		extraData :: String
	}

type DecisionProgram = KnownState -> RobotAction

data RobotAction = MoveIn Direction | Dig | Spawn Direction

data Robot = Robot {robotProgram :: String}
