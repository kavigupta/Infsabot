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

tests :: Test
tests
    = TestLabel "Symmetry Tests" $ TestList
            $ map (\(n, t) -> TestLabel ("Turn " ++ show n) t)
            $ zip [0 ::Int ..]
            $ map assertTeamsSymmetric $ take 1 $ boards params initialBoard

    where
    params :: Parameters
    params = defaultParameters {paramBoardSize = 100, paramInitialMaterial = 1000}
    initialBoard :: Board
    initialBoard = startingBoard params basicProgram

assertTeamsSymmetric :: Board -> Test
assertTeamsSymmetric b
        = TestList [equalLength, TestList $ map assertTeamSymmetry sortedAsBs]
    where
    (as, bs) = partition teamIsA $ boardRobots b
    equalLength = TestCase $ assertEqual
        "Equal numbers of robots" (length as) (length bs)
    adjustedBs = map (\(x, y, rob) -> (boardSize b - x - 1, boardSize b - y - 1, rob)) bs
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
            $ TestList tests
    where
        tests :: [Test]
        tests = map getTests
            $ zip assertLabels results
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

getCoordinates :: (Int, Int, Robot) -> (Int, Int)
getCoordinates (x, y, _) = (x, y)

teamIsA :: (Int, Int, Robot) -> Bool
teamIsA (_, _, rob) = robotTeam rob == A

assertEquivalence :: (Eq a) => (Robot -> a) -> Robot -> Robot -> Bool
assertEquivalence = on (==)
