module Infsabot.Rendering (renderBoard, renderBoardAndActions, renderVictory) where


import Infsabot.Base.Interface(colorOf, RDirection(..), Team, colorOf)
import Infsabot.Tools.Interface(Natural, unNatural)
import Infsabot.Parameters(Parameters(..))
import Infsabot.Board.Interface(Board, GameSpot(..), boardSize, (!!!))
import Infsabot.Robot.Interface(Robot(..))
import Infsabot.RobotAction.Interface(RobotAction(..), FireAction(..), SendAction(..), SpawnAction(..))

import Codec.Picture.Types

import qualified Data.Map as M

import Data.Maybe(fromJust)

renderVictory :: Int -> Maybe Team -> Image PixelRGB8
renderVictory size victor = generateImage (\_ _ -> color) size size
    where color = case victor of
            Nothing -> PixelRGB8 255 255 255
            Just x -> colorOf x

renderBoard :: Parameters -> Int -> Board -> Image PixelRGB8
renderBoard p n b = renderBoardAndActions p n b []

-- Renders the given board as an image
renderBoardAndActions :: Parameters -> Int -> Board -> [((Int, Int), RobotAction)] -> Image PixelRGB8
renderBoardAndActions p n b acts = generateImage colorAt (n * size) (n * size)
    where
    colorAt x y = renderActionSquare p ((blockX, blockY) `M.lookup` actsMap) n (fromJust $ b !!! (blockX, blockY)) offX offY
        where
        (blockX, blockY) = (x `div` n, y `div` n)
        (offX, offY) = (x `mod` n, y `mod` n)
    size = boardSize b
    actsMap = M.fromList acts

renderActionSquare :: Parameters -> Maybe RobotAction -> Int -> GameSpot -> Int -> Int -> PixelRGB8
renderActionSquare p act n g x y = case act of
        Nothing -> background
        (Just a) ->
            if renderAction a n (x, y)
            then PixelRGB8 0 0 0
            else background
    where
    background = renderSquare p n g (x, y)

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
    
onBorderRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
onBorderRectangle minX maxX minY maxY x y
    | x < minX || x > maxX      = False
    | x == minX || x == maxX    = minY <= y && y <= maxY
    | otherwise                 = minY == y || maxY == y

inRectangle :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
inRectangle minX maxX minY maxY x y
    = minX <= x && x <= maxX && minY <= y && y <= maxY
    
renderBar :: Int -> Int -> Int -> Double -> PixelRGB8
renderBar minY maxY y ratio
        | loc < 1 - ratio   = PixelRGB8 255 0 0
        | otherwise         = PixelRGB8 0 255 0
    where
    loc = fromIntegral (y - minY) / fromIntegral (maxY - minY)

divin :: Int -> Natural -> Double
divin x y = fromIntegral x / fromIntegral (unNatural y)

renderSquare :: Parameters -> Int -> GameSpot -> (Int, Int) -> PixelRGB8
renderSquare _ _ (GameSpot spot Nothing) _
    = colorOf spot
renderSquare
        Parameters {paramInitialHP=hpMax, paramInitialMaterial=matMax}
        size
        (GameSpot _ (Just Robot {robotTeam=team, robotAppearance=app, robotHitpoints=hp, robotMaterial=mat})) (x, y)
    | onBorderRectangle leftHP rightHP topBar botBar x y
        = PixelRGB8 0 0 0
    | onBorderRectangle leftM rightM topBar botBar x y
        = PixelRGB8 0 0 0
    | inRectangle leftHP rightHP topBar botBar x y
        = renderBar topBar botBar y (divin hp hpMax)
    | inRectangle leftM rightM topBar botBar x y
        = renderBar topBar botBar y (divin mat matMax)
    | x < size `div` 4 || y < size `div` 4 || x > 3 * size `div` 4 || y > 3 * size `div` 4
        = colorOf app
    | otherwise
        = colorOf team
    where
    (leftHP, rightHP) = (13 * size `div` 16, 14 * size `div` 16)
    (leftM, rightM) = (14 * size `div` 16, 15 * size `div` 16)
    (topBar, botBar) = (size `div` 8, 7 * size `div` 8)
