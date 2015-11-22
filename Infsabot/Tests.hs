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
import Infsabot.Debug

stressTest :: IO ()
stressTest = do
    writePng "temp.png" . renderBoard 1
        $ (boards
                defaultParameters
                $ startingBoard defaultParameters basicProgram)
            !! 200

tests :: Test
tests = TestList [generalGameTest, mcrEdgeCases]

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
    nTurns = 30
    params :: Parameters
    params = defaultParameters {paramBoardSize = 100, paramInitialMaterial = 1000}
    initialBoard :: Board
    initialBoard = startingBoard params basicProgram

mcrEdgeCases :: Test
mcrEdgeCases = TestList $ map (\(n, b) -> TestCase $ assertBool ("MCR Edge Case " ++ show n) b) $ zip [1::Int ..]
        [
            snd $ propConflictsResolved [
                ((-51,49,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 179 12 154}, robotMaterial = 70, robotHitpoints = 74, robotBirthdate = 1, robotMemory = fromList [], robotMessages = []}),Dig),
                ((-51,50,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 106 170 82}, robotMaterial = 10, robotHitpoints = 71, robotBirthdate = 11, robotMemory = fromList [], robotMessages = []}),
                    Spawn {newDirection = W, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 236 64 19}, newMaterial = 53, newMemory = fromList []})
            ],
            snd $ propConflictsResolved [
                ((-38,-32,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 73 119 0}, robotMaterial = 6, robotHitpoints = 33, robotBirthdate = 67, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 149 52 252}, newMaterial = -33, newMemory = fromList []}),
                ((-39,-32,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 20 86 151}, robotMaterial = 7, robotHitpoints = 23, robotBirthdate = 27, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 175 35 78}, newMaterial = 68, newMemory = fromList []})
            ],
            propOrderIndependence removeConflicting [
                ((8,9,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, newMaterial = 333, newMemory = fromList []}),
        	    ((9,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, newMaterial = 333, newMemory = fromList []}),
        	    ((8,10,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = fromList [], robotMessages = []}),MoveIn N),
        	    ((10,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = fromList [], robotMessages = []}),MoveIn N)
                ] 2,
            snd $ propConflictsResolved [
            		((11,0,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 216 199 136}, robotMaterial = 7, robotHitpoints = 16, robotBirthdate = 22, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 176 99 209}, newMaterial = -18, newMemory = fromList []}),
            		((12,0,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 188 125 181}, robotMaterial = 7, robotHitpoints = 4, robotBirthdate = 24, robotMemory = fromList [], robotMessages = []}),Fire {fireDirection = W, materialExpended = -14}),
            		((13,0,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 68 95 208}, robotMaterial = 13, robotHitpoints = 21, robotBirthdate = 10, robotMemory = fromList [], robotMessages = []}),Fire {fireDirection = E, materialExpended = -6}),
            		((10,0,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 51 160 7}, robotMaterial = 13, robotHitpoints = 9, robotBirthdate = 19, robotMemory = fromList [], robotMessages = []}),MoveIn S)
            	],
            snd $ propConflictsResolved [((0,0,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 1}, robotMaterial = 3, robotHitpoints = 1, robotBirthdate = 3, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 0 1 2}, newMaterial = -2, newMemory = fromList []}),((1,2,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 2}, robotMaterial = 2, robotHitpoints = 1, robotBirthdate = 2, robotMemory = fromList [], robotMessages = []}),MoveIn W),((1,1,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 2 0}, robotMaterial = 0, robotHitpoints = 3, robotBirthdate = 2, robotMemory = fromList [], robotMessages = []}),Noop)],
            snd $ propConflictsResolved [((0,0,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 1}, robotMaterial = 3, robotHitpoints = 1, robotBirthdate = 3, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 0 1 2}, newMaterial = -2, newMemory = fromList []}),((1,2,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 2}, robotMaterial = 2, robotHitpoints = 1, robotBirthdate = 2, robotMemory = fromList [], robotMessages = []}),MoveIn W),((1,1,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 2 0}, robotMaterial = 0, robotHitpoints = 3, robotBirthdate = 2, robotMemory = fromList [], robotMessages = []}),Noop)],
            snd $ propConflictsResolved [((-3,8,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 40 45 30}, robotMaterial = 2, robotHitpoints = 8, robotBirthdate = 5, robotMemory = fromList [], robotMessages = []}),MoveIn S),((-3,9,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 50 27 11}, robotMaterial = 16, robotHitpoints = 4, robotBirthdate = 11, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 14 29 55}, newMaterial = -6, newMemory = fromList []})],
            snd $ propConflictsResolved [((-12,45,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 61 19 106}, robotMaterial = 29, robotHitpoints = 40, robotBirthdate = 62, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 35 173 225}, newMaterial = 5, newMemory = fromList []}),((-11,45,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 198 148 144}, robotMaterial = 6, robotHitpoints = 34, robotBirthdate = 34, robotMemory = fromList [], robotMessages = []}),MoveIn W)]
        ]

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
        ("Equal numbers of robots\n\t"
            ++ show (map printRobot as)
            ++ "\n\t"
            ++ show (map printRobot bs))
        (length as) (length bs)
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
