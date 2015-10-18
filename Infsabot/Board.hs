module Infsabot.Board(BoardSpot, Board, startingBoard, renderBoard) where

import Codec.Picture
import Data.RandomAccessList
import Infsabot.MathTools

type RAL = RandomAccessList

data BoardSpot = SpotEmpty | SpotMaterial
	deriving (Eq, Show)

spotColor :: BoardSpot -> PixelRGB8
spotColor SpotEmpty = PixelRGB8 255 255 255 
spotColor SpotMaterial = PixelRGB8 128 128 128 

data Board = Board { 
	boardContents :: (RAL (RAL BoardSpot)),
	boardRobots :: [Robot],
	boardWidth :: Int,
	boardHeight :: Int
}

(!!!) :: Board -> (Int, Int) -> BoardSpot
b !!! (x, y) = boardContents b .!. x .!. y
	where (.!.) = flip Data.RandomAccessList.lookup

startingBoard :: Int -> Board
startingBoard size = Board startingSpots size size
	where
	startingSpots :: (RAL (RAL BoardSpot))
	startingSpots = fmap ys $ fromList [0..size]
		where
		ys x = fmap (initialColor x) $ fromList [0..size]
		initialColor :: Int -> Int -> BoardSpot
		initialColor x y = if isPrime (x + y^2) then SpotMaterial else SpotEmpty

renderBoard :: Board -> Image PixelRGB8
renderBoard b = generateImage (\x y -> spotColor $ b !!! (x, y)) (boardWidth b) (boardHeight b)

