module Infsabot.Board (
		BoardSpot,
		Board,
			boardContents, boardRobots, boardWidth, boardHeight, boardTime,
			(!!!), setRobot, deleteRobot, updateSpot, robotAlongPath,
		startingBoard,
		renderBoard
	) where

import Codec.Picture
import Data.RandomAccessList
import Infsabot.MathTools
import Infsabot.Constants
import Infsabot.Base
import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Parameters
import qualified Data.Map as M

type RAL = RandomAccessList

(.!.) :: RandomAccessList a -> Int -> a
(.!.) = flip Data.RandomAccessList.lookup

-- Gets the display color of the given spot.
-- If there is a robot, then the color is that of the robot
-- Otherwise, the color is that of the underlying material
spotColor :: SeenSpot -> PixelRGB8
spotColor (SeenSpot SpotEmpty Nothing) 		= colorOfEmptySpot
spotColor (SeenSpot SpotMaterial Nothing) 	= colorOfMaterialSpot
spotColor (SeenSpot _ (Just rob)) 			= robotColor rob

-- Represents a board.
data Board = Board {
	-- The contents of the board
	-- In the form of a RAL of RALs of GameSpots, forming a Matrix
	boardContents :: RAL (RAL GameSpot),
	-- The robots on the Board
	boardRobots :: [(Int, Int, Robot)],
	-- The Width of the Board
	boardWidth :: Int,
	-- The Height of the board
	boardHeight :: Int,
	-- The Current Time of the Board
	boardTime :: Int
}

-- Gets the game spot at the given board location
(!!!) :: Board -> (Int, Int) -> GameSpot
b !!! (x, y) = boardContents b .!. x .!. y

-- Sets the game spot at the given board location to the given value
(!->) :: Board -> (Int, Int) -> GameSpot -> Board
(b !-> (x, y)) gs = b {
		boardContents = newcontents,
		boardWidth = max x $ boardWidth b,
		boardHeight = max y $ boardHeight b
	}
	where
	-- The old column x
	oldx = boardContents b .!. x
	-- The updated column x with the new value of y
	newx = update y gs oldx
	-- The updated board with the element at (x, y)
	newcontents = update x newx $ boardContents b

-- Creates a starting square board with a given size
-- This board contains no robots`
startingBoard :: Parameters -> (Team -> RobotProgram) -> Board
startingBoard p programOf = Board {
		boardContents 	= startingSpots,
		boardRobots 	= [	(boardSize p, 0, bot A),
							(0, boardSize p, bot B)],
		boardWidth 		= boardSize p,
		boardHeight 	= boardSize p,
		boardTime 		= 0
	}
	where
	startingSpots :: (RAL (RAL GameSpot))
	startingSpots = fmap ys $ fromList [0..boardSize p]
		where
		ys x = fmap (initialColor x) $ fromList [0..boardSize p]
		initialColor :: Int -> Int -> GameSpot
		initialColor x y =
			if isPrime (x * x + y * y)
				then GameSpot SpotMaterial Nothing
				else GameSpot SpotEmpty Nothing
	bot team = Robot{
		robotProgram = programOf team,
		robotTeam = team,
		robotAppearance = RobotAppearance {robotColor = colorDefaultOf team},
		robotMaterial = paramInitialMaterial p,
		robotHitpoints = paramInitialHP p,
		robotBirthdate = 0,
		robotMemory = M.empty,
		robotMessages = []
	}
-- Adds a robot to the board
-- 		1. places the robot at the Gamespot at the given coordinates
--		2. Adds the robot to the list of robots
setRobot :: (Int, Int, Robot) -> Board -> Board
setRobot (x, y, rob) b = newB {
		boardRobots = (x,y,rob) : boardRobots newB
	}
	where
	GameSpot oldMaterial _ = b !!! (x, y)
	newB = b !-> (x, y) $ GameSpot oldMaterial $ Just rob

deleteRobot :: (Int, Int) -> Board -> Board
deleteRobot (x, y) b = newB {
		boardRobots = Prelude.filter pointNEQ $ boardRobots newB
	}
	where
	pointNEQ (x2,y2,_) = (x /= x2) && (y /= y2)
	GameSpot oldMaterial _ = b !!! (x, y)
	newB = b !-> (x, y) $ GameSpot oldMaterial $ Nothing

updateSpot :: (Int, Int) -> BoardSpot -> Board -> Board
updateSpot (x, y) spot b = newB
	where
	currentRobot :: Maybe Robot
	GameSpot _ currentRobot = b !!! (x, y)
	newB :: Board
	newB = b !-> (x, y) $ GameSpot spot currentRobot

-- Finds the first robot along the given direction from the given position
	-- (but not the robot at the given position)
-- Which may be up to n paces away
robotAlongPath :: Board -> (Int, Int) -> Direction -> Int -> Maybe (Int, Int, Robot)
robotAlongPath _ _ _ 0 = Nothing
robotAlongPath b (x, y) dir n =
		case perhapsRobot of
			Nothing 	-> robotAlongPath b offsettedPosition dir (n-1)
			Just rob 	-> Just $ (x, y, rob)
	where
	offsettedPosition = applyOffset (getOffset dir) (x, y)
	GameSpot _ perhapsRobot = b !!! offsettedPosition

-- Renders the given board as an image
renderBoard :: Board -> Image PixelRGB8
renderBoard b = generateImage colorAt (boardWidth b) (boardHeight b)
	where
	colorAt :: Int -> Int -> PixelRGB8
	colorAt x y = spotColor . toSeenSpot $ b !!! (x, y)
