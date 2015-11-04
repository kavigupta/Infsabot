module Main(createDemoBoards, tests) where

import Test.HUnit

import Infsabot.Board(Board, renderBoard, startingBoard, boardRobots)
import Codec.Picture
import Control.Monad(forM_)
import Infsabot.Parameters
import Infsabot.GamePlay(play)
import Infsabot.RobotStrategy
import Infsabot.Tests(assertTeamsSymmetric)


writeBoard :: String -> Board -> IO ()
writeBoard s = writePng s . renderBoard

createDemoBoards :: Int -> IO ()
createDemoBoards demoBoardSize
    = do
        writeBoard "gen/___demo-starting-board.png" $ snd $ head selectedBoards
        forM_ (tail selectedBoards) $ \(x, board) ->
            do
                writeBoard ("gen/___demo-moves-" ++ (show x) ++ ".png") board
                putStrLn $ show $ boardRobots board
    where
    params = defaultParameters {paramBoardSize = demoBoardSize, paramInitialMaterial=1000}
    selectedBoards = take 30 $ numberedBoards params

tests :: Test
tests = TestList [symmetryTests]
    where
    params :: Parameters
    params = defaultParameters {paramBoardSize = 100, paramInitialMaterial = 1000}
    symmetryTests :: Test
    symmetryTests
        = TestLabel "Symmetry Tests" $ TestList
            $ map (\(n, t) -> TestLabel ("Turn " ++ show n) t)
            $ zip [0 ::Int ..]
            $ map assertTeamsSymmetric $ take 1 $ boards params

numberedBoards :: Parameters -> [(Int, Board)]
numberedBoards params = zip [1..] $ boards params

boards :: Parameters -> [Board]
boards params = iterate (play params) initialBoard
    where
    initialBoard = startingBoard params basicProgram
