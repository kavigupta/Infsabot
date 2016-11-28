module Infsabot.Demoes(demoes) where

import Codec.Picture(writePng)

import Data.Function(on)
import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.Base.Interface
import Infsabot.RobotAction.Interface
import Infsabot.GamePlay.Interface(boards)
import Infsabot.Board.Interface(Board, startingBoard)
import Infsabot.Rendering(renderBoard)
import Infsabot.Strategy.BasicStrategy(basicProgram)
import Infsabot.Strategy.StandardStrategies

import Infsabot.Tools.Interface

import System.Process(system)
import System.Random

fps :: Int
fps = 5

writeBoard :: Int -> String -> Board -> IO ()
writeBoard scale s = writePng s . renderBoard scale

demoes :: IO ()
demoes = do
    writeBoard 16 "demo-starting-board.png" $ startingBoard (defaultParameters {paramBoardSize = makeNatural 60}) basicProgram
    system "mkdir -p ./strategies/random-v-random"
    simulateGame SP {
        nBoards=30,
        boardSize=10,
        pathToImage="./strategies/random-v-random/rvr",
        strategyA=randomMoves (replicate 6 (1/6)) (mkStdGen 0),
        strategyB=randomMoves (replicate 6 (1/6)) (mkStdGen 1)
    }
    simulateGame SP {nBoards=150, boardSize=60, pathToImage="./demo/demo-moves", strategyA=basicProgram A, strategyB=basicProgram B}

data SimulationParams = SP {
    nBoards :: Int,
    boardSize :: Int,
    pathToImage :: String,
    strategyA :: RobotProgram,
    strategyB :: RobotProgram
}

simulateGame :: SimulationParams -> IO ()
simulateGame SP {nBoards=nB, boardSize=size, pathToImage=path, strategyA=sA, strategyB=sB}
    = do
        forM_ (tail selectedBoards) $ \(x, board) -> do
            print x
            writeBoard boardScalingFactor (path ++ "-" ++ showPadded x ++ ".png") board
        forM_ ["mp4", "gif"] (system . ffmpeg)
        return ()
    where
    showPadded :: Int -> String
    showPadded n = replicate (((-) `on` (length . show)) nB n) '0' ++ show n
    boardScalingFactor = 1000 `div` size
    params = defaultParameters {paramBoardSize = makeNatural size, paramInitialMaterial = 100}
    selectedBoards
        = take nB $
            zip [0 :: Int ..] $
            boards params . startingBoard params $ \x ->
                case x of
                    A -> sA
                    B -> sB

    ffmpeg :: String -> String
    ffmpeg ext = "ffmpeg -f image2 -r "
            ++ show fps
            ++ " -pattern_type glob -i "
            ++ "'" ++ path ++ "-*.png' "
            ++ path ++ "."++ ext ++ " -y"
