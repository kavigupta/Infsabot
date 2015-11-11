module Infsabot.Demoes(demoes) where

import Codec.Picture(writePng)

import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.GamePlay(boards)
import Infsabot.Board(Board, renderBoard, startingBoard)
import Infsabot.RobotStrategy(basicProgram)

import System.Process(system)

nBoards :: Int
nBoards = 150

boardSize :: Int
boardSize = 70

boardScalingFactor :: Int
boardScalingFactor = 1000 `div` boardSize

fps :: Int
fps = 5

showPadded :: Int -> String
showPadded n = (take (length (show nBoards) - length str) $ repeat '0') ++ str
    where str = show n

demoes :: IO ()
demoes = createDemoBoards boardSize

createDemoBoards :: Int -> IO ()
createDemoBoards demoBoardSize
    = do
        writeBoard "demo-starting-board.png" $ snd $ head selectedBoards
        forM_ (tail selectedBoards) $ \(x, board) ->
            do
                writeBoard ("demo/demo-moves-" ++ (showPadded x) ++ ".png") board
        system $
            "ffmpeg -f image2 -r " ++ show fps ++ " -pattern_type glob -i './demo/demo-moves-*.png'  demo/demo-moves.mp4 -y"
        return ()
    where
    params = defaultParameters {paramBoardSize = demoBoardSize, paramInitialMaterial=100}
    selectedBoards
        = take nBoards $
            zip [0 :: Int ..] $
            boards params $ startingBoard params basicProgram

writeBoard :: String -> Board -> IO ()
writeBoard s = writePng s . renderBoard boardScalingFactor
