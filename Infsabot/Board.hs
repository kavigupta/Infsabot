module Infsabot.Board (
		BoardSpot,
		Board,
			boardContents, boardRobots, boardWidth, boardHeight, boardTime,
			(!!!),
		startingBoard,
		renderBoard
	) where

import Codec.Picture
import Data.RandomAccessList
import Infsabot.MathTools
import Infsabot.Constants
import Infsabot.Base
import Infsabot.RobotAction
import Infsabot.Robot

type RAL = RandomAccessList

-- Gets the display color of the given spot.
-- If there is a robot, then the color is that of the robot
-- Otherwise, the color is that of the underlying material
spotColor :: SeenSpot -> PixelRGB8
spotColor (SeenSpot SpotEmpty Nothing) 		= colorOfEmptySpot
spotColor (SeenSpot SpotMaterial Nothing) 	= colorOfMaterialSpot
spotColor (SeenSpot _ (Just rob)) 			= robotColor rob

data Board = Board {
	boardContents :: (RAL (RAL GameSpot)),
	boardRobots :: [(Int, Int, Robot)],
	boardWidth :: Int,
	boardHeight :: Int,
	boardTime :: Int
}

(!!!) :: Board -> (Int, Int) -> GameSpot
b !!! (x, y) = boardContents b .!. x .!. y
	where (.!.) = flip Data.RandomAccessList.lookup

startingBoard :: Int -> Board
startingBoard size = Board {
		boardContents 	= startingSpots,
		boardRobots 	= [],
		boardWidth 		= size,
		boardHeight 	= size,
		boardTime 		= 0
	}
	where
	startingSpots :: (RAL (RAL GameSpot))
	startingSpots = fmap ys $ fromList [0..size]
		where
		ys x = fmap (initialColor x) $ fromList [0..size]
		initialColor :: Int -> Int -> GameSpot
		initialColor x y =
			if isPrime (x + y^2)
				then GameSpot SpotMaterial Nothing
				else GameSpot SpotEmpty Nothing

renderBoard :: Board -> Image PixelRGB8
renderBoard b = generateImage colorAt (boardWidth b) (boardHeight b)
	where
	colorAt :: Int -> Int -> PixelRGB8
	colorAt x y = spotColor . toSeenSpot $ b !!! (x, y)
