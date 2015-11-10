module Infsabot.Tests (tests) where

import Test.HUnit
import Data.Function(on)
import Data.List(partition, sortBy)

import Infsabot.Base
import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotStrategy
import Infsabot.GamePlay(boards)
import Infsabot.Parameters
--import Debug.Trace
--import Infsabot.Debug

tests :: Test
tests = TestList [symmetryTests]

symmetryTests :: Test
symmetryTests
        = TestLabel "Symmetry Tests" $ TestList
            $ map (\(n, t) -> TestLabel ("Turn " ++ show n) t)
            $ zip [0 ::Int ..]
            $ map (\b -> TestList [assertTeamsSymmetric b, assertBoardSymmetry b]) $ neededBoards
    where
    neededBoards = take nTurns $ boards params initialBoard
    nTurns = 100
    params :: Parameters
    params = defaultParameters {paramBoardSize = 5, paramInitialMaterial = 1000}
    initialBoard :: Board
    initialBoard = startingBoard params basicProgram

assertTeamsSymmetric :: Board -> Test
assertTeamsSymmetric b
        = TestList [equalLength, TestList $ map assertTeamSymmetry sortedAsBs]
    where
    (as, bs) = partition teamIsA $ boardRobots b
    equalLength = TestCase $ assertEqual
        "Equal numbers of robots" (length as) (length bs)
    adjustedBs = map (\(x, y, rob) -> (y, x, rob)) bs
    sorter =  sortBy (compare `on` getCoordinates)
    sortedAsBs = zip (sorter as) (sorter adjustedBs)

assertTeamSymmetry :: ((Int, Int, Robot), (Int, Int, Robot)) -> Test
assertTeamSymmetry ((x1, y1, rob1), (x2, y2, rob2))
    | x1 /= x2 || y1 /= y2
        = TestCase . assertFailure
            $  "The Team A robot at "
            ++ show (x1, y1)
            ++ " appears to have no corresponding B robot"
    | otherwise
        = TestLabel ("The robots at "
                ++ show (x1, y1) ++ " are not equivalent")
            $ TestList $ map getTests
                $ zip assertLabels results
    where
        getTests :: (String, Bool) -> Test
        getTests (label, result) = TestCase $ assertBool label result
        results :: [Bool]
        results = map (\u -> u rob1 rob2) assertions
        assertions :: [Robot -> Robot -> Bool]
        assertions = [assertEquivalence robotMaterial,
                        assertEquivalence robotHitpoints,
                        assertEquivalence robotBirthdate,
                        assertEquivalence robotMemory,
                        assertEquivalence robotMessages]
        assertLabels = map ("Different "++) [
                "material",
                "hitpoints",
                "birthdate",
                "memory",
                "messages"
            ]
assertBoardSymmetry :: Board -> Test
assertBoardSymmetry b = TestList $ map symmetric $ zip [0.. boardSize b - 1] [0..boardSize b - 1]
    where
    symmetric (x, y) = case u of
            Nothing -> TestCase $ assertBool "" True
            (Just value) -> TestCase $ assertBool
                ("The coordinates at " ++ show ((x, y), (y, x))++ " do not match") value
        where
        u = do
            (GameSpot regular _) <- b !!! (x, y)
            (GameSpot other _) <- b !!! (y, x)
            return $ regular == other
getCoordinates :: (Int, Int, Robot) -> (Int, Int)
getCoordinates (x, y, _) = (x, y)

teamIsA :: (Int, Int, Robot) -> Bool
teamIsA (_, _, rob) = robotTeam rob == A

assertEquivalence :: (Eq a) => (Robot -> a) -> Robot -> Robot -> Bool
assertEquivalence = on (==)
