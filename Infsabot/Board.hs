module Infsabot.Board(BoardSpot, Board, startingBoard, renderBoard) where

import Codec.Picture
import Data.RandomAccessList
import Infsabot.MathTools
import Infsabot.Robot

type RAL = RandomAccessList

data BoardSpot = SpotEmpty | SpotMaterial
	deriving (Eq, Show)

spotColor :: BoardSpot -> PixelRGB8
spotColor SpotEmpty = PixelRGB8 255 255 255
spotColor SpotMaterial = PixelRGB8 128 128 128

data Board = Board {
	boardContents :: (RAL (RAL (BoardSpot, Maybe Robot))),
	boardRobots :: (Int, Int, Robot),
	boardWidth :: Int,
	boardHeight :: Int,
	boardTime :: Int
}

(!!!) :: Board -> (Int, Int) -> (BoardSpot, Maybe Robot)
b !!! (x, y) = boardContents b .!. x .!. y
	where (.!.) = flip Data.RandomAccessList.lookup

asSeenSpot :: (BoardSpot, Maybe Robot) -> SeenSpot
asSeenSpot (boardSpot, Nothing) = SeenSpot {boardSpot = boardSpot, robotAt = Nothing}
asSeenSpot (boardSpot, Just robot) = SeenSpot {boardSpot = boardSpot, robotAt = Just (robotAppearan	ce robot)}

startingBoard :: Int -> Board
startingBoard size = Board startingSpots size size
	where
	startingSpots :: (RAL (RAL BoardSpot))
	startingSpots = fmap ys $ fromList [0..size]
		where
		ys x = fmap (initialColor x) $ fromList [0..size]
		initialColor :: Int -> Int -> BoardSpot
		initialColor x y = if isPrime (x + y^2) then SpotMaterial else SpotEmpty

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

renderBoard :: Board -> Image PixelRGB8
renderBoard b = generateImage (\x y -> spotColor . fst $ b !!! (x, y)) (boardWidth b) (boardHeight b)
