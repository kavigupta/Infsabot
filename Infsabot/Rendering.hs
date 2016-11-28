module Infsabot.Rendering (renderBoard) where


import Infsabot.Base.Interface(colorOf)
import Infsabot.Board.Interface(Board, GameSpot(..), boardSize, (!!!))
import Infsabot.Robot.Interface(Robot(..))

import Codec.Picture.Types

import Data.Maybe(fromJust)
import Control.Monad.ST
import Control.Monad

-- Renders the given board as an image
renderBoard :: Int -> Board -> Image PixelRGB8
renderBoard n b = runST $ do
        img <- newMutableImage (n * boardSize b) (n * boardSize b)
        forM_ [0..boardSize b - 1] $ \x ->
            forM_ [0..boardSize b - 1] $ \y -> do
                let square = renderSquare n . fromJust $ b !!! (x, y)
                forM_ [0..n-1] $ \offx ->
                    forM_ [0..n-1] $ \offy ->
                        writePixel img (x * n + offx) (y * n + offy) (square offx offy)
        unsafeFreezeImage img

renderSquare :: Int -> GameSpot -> Int -> Int -> PixelRGB8
renderSquare _ (GameSpot spot Nothing) _ _
    = colorOf spot
renderSquare size (GameSpot _ (Just Robot {robotTeam=team, robotAppearance=app})) x y
    | x < size `div` 4 || y < size `div` 4 || x > 3 * size `div` 4 || y > 3 * size `div` 4
        = colorOf app
    | otherwise
        = colorOf team
