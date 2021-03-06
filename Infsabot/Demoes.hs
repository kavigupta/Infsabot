module Infsabot.Demoes(demoes) where

import Codec.Picture(writePng)

import Data.Function(on, fix)
import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.Base.Interface
import Infsabot.RobotAction.Interface
import Infsabot.GamePlay.Interface(boardsAndActions, Game(..), limit)
import Infsabot.Board.Interface(Board, startingBoard)
import Infsabot.Rendering(renderBoardAndActions, renderVictory)
import Infsabot.Strategy.BasicStrategy(basicProgram)
import Infsabot.Strategy.StandardStrategies

import Infsabot.Tools.Interface

import System.Process(system)
import System.Random
import System.FilePath

fps :: Int
fps = 5

writeBoard :: Parameters -> Int -> String -> (Board, [((Int, Int), RobotAction)]) -> IO ()
writeBoard p scale s = writePng s . renderer
    where
    renderer (x, y) = renderBoardAndActions p scale x y

writeOnlyBoard :: Parameters -> Int -> String -> Board -> IO ()
writeOnlyBoard p scale s b = writeBoard p scale s (b, [])

demoes :: IO ()
demoes = do
    writeOnlyBoard defaultParameters 16 "demo-starting-board.png" $ startingBoard (defaultParameters {paramBoardSize = makeNatural 60}) basicProgram
    system "mkdir -p ./strategies/random-v-random"
    simulateGame SP {
        nBoards=100,
        boardSize=6,
        pathToImage="./strategies/digger-v-digger/dvd",
        strategyA=fix $ \d -> digger $ randomMoves (Just d) (0:repeat (1/3)) (mkStdGen 0),
        strategyB=fix $ \d -> digger $ randomMoves (Just d) (0:repeat (1/3)) (mkStdGen 2)
    }
    simulateGame SP {
        nBoards=30,
        boardSize=10,
        pathToImage="./strategies/random-v-random/rvr",
        strategyA=randomMoves Nothing (replicate 6 (1/6)) (mkStdGen 0),
        strategyB=randomMoves Nothing (replicate 6 (1/6)) (mkStdGen 4)
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
        system $ "rm -r " ++ takeDirectory path
        system $ "mkdir -p " ++ takeDirectory path
        forM_ selectedBoards $ \(x, board) -> do
            print x
            writeBoard params boardScalingFactor (path ++ "-" ++ showPadded x ++ ".png") board
        writePng (path ++ "-" ++ showPadded (length selectedBoards) ++ ".png") $ renderVictory 1000 victor
        system . ffmpeg $ ["mp4", "gif"]
        return ()
    where
    showPadded :: Int -> String
    showPadded n = replicate (((-) `on` (length . show)) (length selectedBoards) n) '0' ++ show n
    boardScalingFactor = 1000 `div` size
    params = defaultParameters {paramBoardSize = makeNatural size, paramInitialMaterial = 100}
    victor :: Maybe Team
    (victor, selectedBoards)
        = limit nB $ labelGame $
            boardsAndActions params . startingBoard params $ \x ->
                case x of
                    A -> sA
                    B -> sB

    ffmpeg :: [String] -> String
    ffmpeg exts = "ffmpeg -f image2 -r "
            ++ show fps
            ++ " -pattern_type glob -i "
            ++ "'" ++ path ++ "-*.png' "
            ++ unwords (map (\ext -> path ++ "."++ ext ++ " -y") exts)

labelGame :: Game a -> Game (Int, a)
labelGame = zipGameFrom 0
    where
    zipGameFrom _ (Victory t) = Victory t
    zipGameFrom n (a :~ g) = (n, a) :~ zipGameFrom (n + 1) g
