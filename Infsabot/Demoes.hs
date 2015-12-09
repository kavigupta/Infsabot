module Infsabot.Demoes(demoes) where

import Codec.Picture(writePng)

import Data.Function(on)
import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.GamePlay.Interface(boards)
import Infsabot.Board.Interface(Board, startingBoard)
import Infsabot.Rendering(renderBoard)
import Infsabot.RobotStrategy(basicProgram)

import Infsabot.Tools.Interface

import System.Process(system)

nBoards, boardSize, boardScalingFactor, fps :: Int

nBoards = 150
boardSize = 60
boardScalingFactor = 1000 `div` boardSize
fps = 5

showPadded :: Int -> String
showPadded n = replicate (((-) `on` (length . show)) nBoards n) '0' ++ show n

demoes :: IO ()
demoes = createDemoBoards boardSize

createDemoBoards :: Int -> IO ()
createDemoBoards demoBoardSize
    = do
        writeBoard "demo-starting-board.png" $ snd $ head selectedBoards
        forM_ (tail selectedBoards) $ \(x, board) -> do
            print x
            writeBoard ("demo/demo-moves-" ++ showPadded x ++ ".png") board
        forM_ ["mp4", "gif"] (system . ffmpeg)
        return ()
    where
    params = defaultParameters {paramBoardSize = makeNatural demoBoardSize, paramInitialMaterial = 100}
    selectedBoards
        = take nBoards $
            zip [0 :: Int ..] $
            boards params $ startingBoard params basicProgram

writeBoard :: String -> Board -> IO ()
writeBoard s = writePng s . renderBoard boardScalingFactor

ffmpeg :: String -> String
ffmpeg ext = "ffmpeg -f image2 -r " ++ show fps ++ " -pattern_type glob -i './demo/demo-moves-*.png'  demo/demo-moves."++ ext ++ " -y"
