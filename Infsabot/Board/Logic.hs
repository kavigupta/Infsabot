module Infsabot.Board.Logic (
		Board(Board),
			boardContents, boardRobots, boardSize, boardTime,
			(!!!), setRobot, robotAt, updateSpot, robotAlongPath, inBoard,
			listOfRobots,
		startingBoard,
		GameSpot(GameSpot), toSeenSpot
	) where

import Data.RandomAccessList(RandomAccessList, lookup, update, fromList)
import Infsabot.Tools.Interface(isPrime)
import Infsabot.Base.Interface
import Infsabot.Robot
import Infsabot.RobotAction.Interface
import Infsabot.Parameters
import qualified Data.Map as M

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data GameSpot = GameSpot BoardSpot (Maybe Robot) deriving (Show)

-- Converts a GameSpot to a seen spot
toSeenSpot :: GameSpot -> SeenSpot
toSeenSpot (GameSpot s Nothing) = SeenSpot s Nothing
toSeenSpot (GameSpot s (Just rob)) = SeenSpot s $ Just $ robotAppearance rob

-- Represents a board.
data Board = Board {
	-- The contents of the board
	-- In the form of a RAL of RALs of GameSpots, forming a Matrix
	boardContents :: RAL (RAL GameSpot),
	-- The robots on the Board
	boardRobots :: M.Map (Int, Int) Robot,
	-- The size of the Board
	boardSize :: Int,
	-- The Current Time of the Board
	boardTime :: Int
} deriving (Show)

-- Gets the game spot at the given board location
(!!!) :: Board -> (Int, Int) -> Maybe GameSpot
b !!! (x, y)
	| inBoard b (x, y)	= Just $ boardContents b .!. x .!. y
	| otherwise			= Nothing

-- Sets the game spot at the given board location to the given value
(!->) :: Board -> (Int, Int) -> GameSpot -> Board
(b !-> (x, y)) gs
	| inBoard b (x, y)	= b {boardContents = newcontents}
	| otherwise			= b
	where
	-- The old column x
	oldx = boardContents b .!. x
	-- The updated column x with the new value of y
	newx = update y gs oldx
	-- The updated board with the element at (x, y)
	newcontents = update x newx $ boardContents b

-- Creates a starting square board with a given size
-- This board contains one robot from each team.
startingBoard :: Parameters -> (Team -> RobotProgram) -> Board
startingBoard p programOf
	= setRobot (0, paramBoardSize p - 1) (bot B) $
		setRobot (paramBoardSize p - 1, 0) (bot A) $
		Board {
			boardContents 	= startingSpots,
			boardRobots 	= M.fromList [],
			boardSize 		= paramBoardSize p,
			boardTime 		= 0
		}
	where
	startingSpots :: (RAL (RAL GameSpot))
	startingSpots = fmap ys $ fromList [0..paramBoardSize p]
		where
		ys x = fmap initialColor $ fromList [0..paramBoardSize p]
			where
			initialColor :: Int -> GameSpot
			initialColor y =
				if isPrime (x * x + y * y)
					then GameSpot SpotMaterial Nothing
					else GameSpot SpotEmpty Nothing
	bot team = Just $ defaultRobot p team (programOf team)

-- Sets the robot at the given spot to the given value, or deletes it.
-- 		1. places the robot at the gamespot at the given coordinates
--		2. Adds the robot to the list of robots
setRobot :: (Int, Int) -> Maybe Robot -> Board -> Board
setRobot (x, y) rob b = delRobot $ b !!! (x, y)
	where
	delRobot Nothing = b
	delRobot (Just (GameSpot oldMaterial _))
			= newB {boardRobots = newRobots rob}
		where
		newB = b !-> (x, y) $ GameSpot oldMaterial rob
		newRobots Nothing 		=  M.delete (x, y) oldRobots
		newRobots (Just robot) 	= M.insert (x, y) robot oldRobots
		oldRobots = boardRobots newB

--Updates the given spot to the new value
updateSpot :: (Int, Int) -> BoardSpot -> Board -> Board
updateSpot (x, y) spot b = b !-> (x, y) $ GameSpot spot (robotAt b (x, y))

-- Gets the robot at the given position, if it exists
robotAt :: Board -> (Int, Int) -> Maybe Robot
robotAt b pos = (b !!! pos) >>= (\(GameSpot _ rob) -> rob)

-- Finds the first robot along the given direction from the given position
	-- (but not the robot at the given position)
-- Which may be up to n paces away
robotAlongPath :: Team -> Board -> (Int, Int) -> RDirection -> Int -> Maybe (Int, Int, Robot)
robotAlongPath _ _ _ _ 0 = Nothing
robotAlongPath team b (x, y) dir n
	= case robotAt b (x, y) of
		Nothing 	-> robotAlongPath team b (applyDirection team dir (x, y)) dir (n-1)
		Just rob 	-> Just (x, y, rob)

-- Returns true iff the given coordinate pair is in the board
inBoard :: Board -> (Int, Int) -> Bool
inBoard b (x, y) = x >= 0 && x < boardSize b && y >= 0 && y < boardSize b

listOfRobots :: Board -> [PositionedRobot]
listOfRobots b = map PositionedRobot $ M.toList $ boardRobots b

type RAL = RandomAccessList

(.!.) :: RandomAccessList a -> Int -> a
(.!.) = flip Data.RandomAccessList.lookup
