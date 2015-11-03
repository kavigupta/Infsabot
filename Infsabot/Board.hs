module Infsabot.Board (
		BoardSpot,
		Board,
			boardContents, boardRobots, boardSize, boardTime,
			(!!!), setRobot, updateSpot, robotAlongPath,
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
	-- The size of the Board
	boardSize :: Int,
	-- The Current Time of the Board
	boardTime :: Int
} deriving (Show)

-- Returns true iff the given coordinate pair is in the board
inBoard :: Board -> (Int, Int) -> Bool
inBoard b (x, y) = x >= 0 && x < boardSize b && y >= 0 && y < boardSize b

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
	= setRobot (0, paramBoardSize p - 1, Just $ bot B) $
		setRobot (paramBoardSize p - 1, 0, Just $ bot A) $
		Board {
			boardContents 	= startingSpots,
			boardRobots 	= [],
			boardSize 		= paramBoardSize p,
			boardTime 		= 0
		}
	where
	startingSpots :: (RAL (RAL GameSpot))
	startingSpots = fmap ys $ fromList [0..paramBoardSize p]
		where
		ys x = fmap (initialColor x) $ fromList [0..paramBoardSize p]
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

-- Sets the robot at the given spot to the given value, or deletes it.
-- 		1. places the robot at the gamespot at the given coordinates
--		2. Adds the robot to the list of robots
setRobot :: (Int, Int, Maybe Robot) -> Board -> Board
setRobot (x, y, rob) b = delRobot $ b !!! (x, y)
	where
	delRobot Nothing = b
	delRobot (Just (GameSpot oldMaterial _))
			= newB {boardRobots = newRobots rob}
		where
		pointNEQ (x2,y2,_) = (x, y) /= (x2, y2)
		newB = b !-> (x, y) $ GameSpot oldMaterial rob
		oldRemoved = Prelude.filter pointNEQ $ boardRobots newB
		newRobots Nothing = oldRemoved
		newRobots (Just robot) = (x, y, robot) : oldRemoved

--Updates the given spot to the new value
updateSpot :: (Int, Int) -> BoardSpot -> Board -> Board
updateSpot (x, y) spot b = b !-> (x, y) $ GameSpot spot (robotAt b (x, y))

-- Gets the robot at the given position, if it exists
robotAt :: Board -> (Int, Int) -> Maybe (Robot)
robotAt b pos = (b !!! pos) >>= (\(GameSpot _ rob) -> rob)

-- Finds the first robot along the given direction from the given position
	-- (but not the robot at the given position)
-- Which may be up to n paces away
robotAlongPath :: Team -> Board -> (Int, Int) -> RDirection -> Int -> Maybe (Int, Int, Robot)
robotAlongPath _ _ _ _ 0 = Nothing
robotAlongPath team b (x, y) dir n
	= case perhapsRobot of
		Nothing 	-> robotAlongPath team b offsettedPosition dir (n-1)
		Just rob 	-> Just $ (x, y, rob)
	where
	offsettedPosition :: (Int, Int)
	offsettedPosition = applyOffset (getOffset team dir) (x, y)
	perhapsRobot = robotAt b (x, y)

-- Renders the given board as an image
renderBoard :: Board -> Image PixelRGB8
renderBoard b = generateImage colorAt (boardSize b) (boardSize b)
	where
	colorAt :: Int -> Int -> PixelRGB8
	colorAt x y = spotColor . toSeenSpot . unpack $ b !!! (x, y)
