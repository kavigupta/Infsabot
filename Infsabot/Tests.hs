module Infsabot.Tests (tests, stressTest) where

import Test.HUnit
import Data.Function(on)
import Data.List(partition, sortBy)

import Infsabot.Base
import Infsabot.Tools
import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotStrategy
import Infsabot.TestLibrary
import Infsabot.RobotAction
import Infsabot.MoveConflictResolution
import Data.Map(fromList)
import Codec.Picture
import Infsabot.GamePlay(boards)
import Infsabot.Parameters

--import Debug.Trace
--import Infsabot.Debug

stressTest :: IO ()
stressTest = do
    writePng "temp.png" . renderBoard 1
        $ (boards
                defaultParameters
                $ startingBoard defaultParameters basicProgram)
            !! 200

tests :: Test
tests = TestList [generalGameTest, mcrEdgeCase]

generalGameTest :: Test
generalGameTest
        = TestLabel "Symmetry Tests" $ TestList
            $ map (\(n, t) -> TestLabel ("Turn " ++ show n) t)
            $ zip [0 ::Int ..]
            $ map (\b -> TestList [
                assertTeamsSymmetric b,
                assertBoardSymmetry b,
                assertRobotSourcesAgree b]) $ neededBoards
    where
    neededBoards = take nTurns $ boards params initialBoard
    nTurns = 45
    params :: Parameters
    params = defaultParameters {paramBoardSize = 35, paramInitialMaterial = 1000}
    initialBoard :: Board
    initialBoard = startingBoard params basicProgram

mcrEdgeCase :: Test
mcrEdgeCase = TestCase $ assertBool "MCR Edge case"
        $ propOrderIndependence removeConflicting [
        	((8,9,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, newMaterial = 333, newMemory = fromList []}),
        	((9,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, newMaterial = 333, newMemory = fromList []}),
        	((8,10,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = fromList [], robotMessages = []}),MoveIn N),
        	((10,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = fromList [], robotMessages = []}),MoveIn N)
            ] 2

assertRobotSourcesAgree :: Board -> Test
assertRobotSourcesAgree b
    = TestCase $
        assertBool "Robot sources boardRobots and boardContents agree" $
        sameElements (boardRobots b) (robotsOnBoard b)

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
                ++ show (x1, y1) ++ " are not equivalent;")
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
