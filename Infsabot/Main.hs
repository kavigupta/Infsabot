module Main(createDemoBoards) where

import Infsabot.Board(Board, renderBoard, startingBoard, boardRobots)
import Codec.Picture
import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.GamePlay(play)
import Infsabot.RobotStrategy


writeBoard :: String -> Board -> IO ()
writeBoard s = writePng s . renderBoard

createDemoBoards :: Int -> IO ()
createDemoBoards demoBoardSize
    = do
        writeBoard "./demo-starting-board.png" initialBoard
        forM_ [1..10] $ \x ->
            do
                let board = fullGame x initialBoard
                writeBoard ("./demo-moves-" ++ (show x) ++ ".png") board
                putStrLn $ show $ boardRobots board
    where
    params = defaultParameters {paramBoardSize = demoBoardSize, paramInitialMaterial=1000}
    initialBoard = startingBoard
        params
        basicProgram
    fullGame :: Int -> Board -> Board
    fullGame turns = (!! turns) . iterate (play params)
