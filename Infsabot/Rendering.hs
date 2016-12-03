module Infsabot.Rendering (renderBoard, renderBoardAndActions) where


import Infsabot.Base.Interface(colorOf, RDirection(..))
import Infsabot.Board.Interface(Board, GameSpot(..), boardSize, (!!!))
import Infsabot.Robot.Interface(Robot(..))
import Infsabot.RobotAction.Interface(RobotAction(..), FireAction(..), SendAction(..), SpawnAction(..))

import Codec.Picture.Types

import qualified Data.Map as M

import Data.Maybe(fromJust)

renderBoard :: Int -> Board -> Image PixelRGB8
renderBoard n b = renderBoardAndActions n b []

-- Renders the given board as an image
renderBoardAndActions :: Int -> Board -> [((Int, Int), RobotAction)] -> Image PixelRGB8
renderBoardAndActions n b acts = generateImage colorAt (n * size) (n * size)
    where
    colorAt x y = renderActionSquare ((blockX, blockY) `M.lookup` actsMap) n (fromJust $ b !!! (blockX, blockY)) offX offY
        where
        (blockX, blockY) = (x `div` n, y `div` n)
        (offX, offY) = (x `mod` n, y `mod` n)
    size = boardSize b
    actsMap = M.fromList acts

renderActionSquare :: Maybe RobotAction -> Int -> GameSpot -> Int -> Int -> PixelRGB8
renderActionSquare act n g x y = case act of
        Nothing -> background
        (Just a) ->
            if renderAction a n (x, y)
            then PixelRGB8 0 0 0
            else background
    where
    background = renderSquare n g (x, y)

renderAction :: RobotAction -> Int -> (Int, Int) -> Bool
renderAction a n (x, y) = case a of
        Noop    -> False
        Die     -> abs (x - y) * 8 < n || abs (x + y - n) * 8 < n
        Fire FireAction {fireDirection=dir}
                -> offX * offX + offY * offY < n * n `div` 64
                        || renderAction (MoveIn dir) n (x, y)
        Send SendAction {sendDirection=dir}
                -> max offX offY < n `div` 8 && min offX offY > -n `div` 8
                        || renderAction (MoveIn dir) n (x, y)
        MoveIn N
                -> (7 * n `div` 16 < x && x < 9 * n `div` 16 || y > 3 * n `div` 4)
                    && abs(x + y) < 3 * n `div` 2 && abs(x - y - n) < 3 * n `div` 2
        MoveIn d
                -> renderAction (MoveIn N) n $ rotate d
        Dig
                -> x + (n-y) > 3 * n `div` 4 && x - (n-y) < n `div` 4 && y > 5 * n `div` 16
        Spawn SpawnAction {newDirection=N}
                -> y > 6 * n `div` 16 && y < 8 * n `div` 16 && abs offX < 7 * n `div` 16
                    || renderAction (MoveIn N) n (x, y)
        Spawn s
                -> renderAction (Spawn s {newDirection=N}) n . rotate . newDirection $ s
    where
    offX = x - n `div` 2
    offY = y - n `div` 2
    rotate N = (x, y)
    rotate E = (y, n-x)
    rotate S = (n-x, n-y)
    rotate W = (n-y, x)

renderSquare :: Int -> GameSpot -> (Int, Int) -> PixelRGB8
renderSquare _ (GameSpot spot Nothing) _
    = colorOf spot
renderSquare size (GameSpot _ (Just Robot {robotTeam=team, robotAppearance=app})) (x, y)
    | x < size `div` 4 || y < size `div` 4 || x > 3 * size `div` 4 || y > 3 * size `div` 4
        = colorOf app
    | otherwise
        = colorOf team
