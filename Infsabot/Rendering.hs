module Infsabot.Rendering (renderBoard) where


import Infsabot.Constants(colorOfEmptySpot, colorOfMaterialSpot)
import Infsabot.Base(SeenSpot(SeenSpot), BoardSpot(SpotMaterial, SpotEmpty), robotColor)
import Infsabot.Robot(toSeenSpot)
import Infsabot.Board(Board, boardSize, (!!!), )
import Codec.Picture(PixelRGB8, Image, generateImage)
import Data.Maybe(fromJust)

-- Renders the given board as an image
renderBoard :: Int -> Board -> Image PixelRGB8
renderBoard n b = generateImage colorAt (n * boardSize b) (n * boardSize b)
	where
	colorAt :: Int -> Int -> PixelRGB8
	colorAt x y = spotColor . toSeenSpot . fromJust $ b !!! (x `div` n, y `div` n)
	-- Gets the display color of the given spot.
	-- If there is a robot, then the color is that of the robot
	-- Otherwise, the color is that of the underlying material
	spotColor :: SeenSpot -> PixelRGB8
	spotColor (SeenSpot SpotEmpty Nothing) 		= colorOfEmptySpot
	spotColor (SeenSpot SpotMaterial Nothing) 	= colorOfMaterialSpot
	spotColor (SeenSpot _ (Just rob)) 			= robotColor rob
