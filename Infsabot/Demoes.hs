module Infsabot.Demoes(demoes) where

import Codec.Picture(writePng)

import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.GamePlay(boards)
import Infsabot.Board(Board, renderBoard, startingBoard)
import Infsabot.RobotStrategy(basicProgram)

demoes :: IO ()
demoes = createDemoBoards 100

createDemoBoards :: Int -> IO ()
createDemoBoards demoBoardSize
    = do
        writeBoard "demo-starting-board.png" $ snd $ head selectedBoards
        forM_ (tail selectedBoards) $ \(x, board) ->
            do
                writeBoard ("demo/demo-moves-" ++ (show x) ++ ".png") board
    where
    params = defaultParameters {paramBoardSize = demoBoardSize, paramInitialMaterial=1000}
    selectedBoards
        = take 500 $
            zip [0 :: Int ..] $
            boards params $ startingBoard params basicProgram

writeBoard :: String -> Board -> IO ()
writeBoard s = writePng s . renderBoard 5
