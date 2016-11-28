module Infsabot.Rendering (renderBoard) where


import Infsabot.Base.Interface(colorOf)
import Infsabot.Board.Interface(Board, GameSpot(..), boardSize, (!!!))
import Infsabot.Robot.Interface(Robot(..))

import Codec.Picture.Types

import Data.Maybe(fromJust)

-- Renders the given board as an image
renderBoard :: Int -> Board -> Image PixelRGB8
renderBoard n b = generateImage colorAt (n * size) (n * size)
    where
    colorAt x y = renderSquare n (fromJust $ b !!! (blockX, blockY)) offX offY
        where
        (blockX, blockY) = (x `div` n, y `div` n)
        (offX, offY) = (x `mod` n, y `mod` n)
    size = boardSize b

renderSquare :: Int -> GameSpot -> Int -> Int -> PixelRGB8
renderSquare _ (GameSpot spot Nothing) _ _
    = colorOf spot
renderSquare size (GameSpot _ (Just Robot {robotTeam=team, robotAppearance=app})) x y
    | x < size `div` 4 || y < size `div` 4 || x > 3 * size `div` 4 || y > 3 * size `div` 4
        = colorOf app
    | otherwise
        = colorOf team
