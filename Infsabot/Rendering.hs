module Infsabot.Rendering (renderBoard) where


import Infsabot.Base.Interface(colorOf)
import Infsabot.Board.Interface(Board, boardSize, (!!!), toSeenSpot)
import Codec.Picture(PixelRGB8, Image, generateImage)
import Data.Maybe(fromJust)

-- Renders the given board as an image
renderBoard :: Int -> Board -> Image PixelRGB8
renderBoard n b = generateImage colorAt (n * boardSize b) (n * boardSize b)
	where
	colorAt :: Int -> Int -> PixelRGB8
	colorAt x y = colorOf . toSeenSpot . fromJust $ b !!! (x `div` n, y `div` n)
