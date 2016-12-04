module Infsabot.Test.CollatedTests (tests, checks, stressTest) where

import Test.HUnit

import Infsabot.Base.Interface
import Infsabot.RobotAction.Interface
import Infsabot.Board.Interface
import Infsabot.Tools.Interface
import Infsabot.Robot.Interface
import Infsabot.Strategy.Random.Interface

import Infsabot.Strategy.BasicStrategy
import Infsabot.Test.TestLibrary
import Infsabot.MoveConflictResolution.Interface
import Data.Map(fromList)
import Codec.Picture
import Infsabot.GamePlay.Interface
import Infsabot.Parameters
import Infsabot.Rendering(renderBoard)
import Test.QuickCheck.Test(isSuccess, Result(Success))
import Control.Monad(liftM2)

import System.Process

stressTest :: IO ()
stressTest = do
    writePng "temp.png" . renderBoard 1 . (!! 200) . snd . limit 1000
        $ boards
                defaultParameters
                (startingBoard defaultParameters basicProgram)
    _ <- system "rm temp.png"
    return ()

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
                assertRobotSourcesAgree b]) neededBoards
    where
    neededBoards = snd . limit nTurns $ boards params initialBoard
    nTurns = 100
    params :: Parameters
    params = defaultParameters {paramBoardSize = 60, paramInitialMaterial = 1000}
    initialBoard :: Board
    initialBoard = startingBoard params basicProgram

mcrEdgeCases :: Test
mcrEdgeCases = TestList $ zipWith createCase [1::Int ..]
        [
            [(PositionedRobot ((-51,49), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 179 12 154, robotMaterial = 70, robotHitpoints = 74, robotBirthdate = 1, robotMemory = InternalState (fromList []), robotMessages = []}),Dig),(PositionedRobot ((-51,50), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 106 170 82, robotMaterial = 10, robotHitpoints = 71, robotBirthdate = 11, robotMemory = InternalState (fromList []), robotMessages = []}), Spawn SpawnAction {newDirection = W, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 236 64 19, newMaterial = 53, newMemory = InternalState (fromList [])})],
            [(PositionedRobot ((-38,-32), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 73 119 0, robotMaterial = 6, robotHitpoints = 33, robotBirthdate = 67, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 149 52 252, newMaterial = -33, newMemory = InternalState (fromList [])}),(PositionedRobot ((-39,-32), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 20 86 151, robotMaterial = 7, robotHitpoints = 23, robotBirthdate = 27, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 175 35 78, newMaterial = 68, newMemory = InternalState (fromList [])})],
            [(PositionedRobot ((8,9), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 0 0 128, newMaterial = 333, newMemory = InternalState (fromList [])}),(PositionedRobot ((9,8), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 255 0 0, newMaterial = 333, newMemory = InternalState (fromList [])}),(PositionedRobot ((8,10), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn N),(PositionedRobot ((10,8), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn N)],
            [(PositionedRobot ((11,0), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 216 199 136, robotMaterial = 7, robotHitpoints = 16, robotBirthdate = 22, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 176 99 209, newMaterial = -18, newMemory = InternalState (fromList [])}),(PositionedRobot ((12,0), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 188 125 181, robotMaterial = 7, robotHitpoints = 4, robotBirthdate = 24, robotMemory = InternalState (fromList []), robotMessages = []}),Fire FireAction {fireDirection = W, materialExpended = -14}),(PositionedRobot ((13,0), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 68 95 208, robotMaterial = 13, robotHitpoints = 21, robotBirthdate = 10, robotMemory = InternalState (fromList []), robotMessages = []}),Fire FireAction {fireDirection = E, materialExpended = -6}),(PositionedRobot ((10,0), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 51 160 7, robotMaterial = 13, robotHitpoints = 9, robotBirthdate = 19, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn S)],
            [(PositionedRobot ((0,0), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 0 0 1, robotMaterial = 3, robotHitpoints = 1, robotBirthdate = 3, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 0 1 2, newMaterial = -2, newMemory = InternalState (fromList [])}),(PositionedRobot ((1,2), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 0 0 2, robotMaterial = 2, robotHitpoints = 1, robotBirthdate = 2, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W),(PositionedRobot ((1,1), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 2 0, robotMaterial = 0, robotHitpoints = 3, robotBirthdate = 2, robotMemory = InternalState (fromList []), robotMessages = []}),Noop)],
            [(PositionedRobot ((0,0), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 0 0 1, robotMaterial = 3, robotHitpoints = 1, robotBirthdate = 3, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 0 1 2, newMaterial = -2, newMemory = InternalState (fromList [])}),(PositionedRobot ((1,2), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 0 0 2, robotMaterial = 2, robotHitpoints = 1, robotBirthdate = 2, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W),(PositionedRobot ((1,1), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 2 0, robotMaterial = 0, robotHitpoints = 3, robotBirthdate = 2, robotMemory = InternalState (fromList []), robotMessages = []}),Noop)],
            [(PositionedRobot ((-3,8), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 40 45 30, robotMaterial = 2, robotHitpoints = 8, robotBirthdate = 5, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn S),(PositionedRobot ((-3,9), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 50 27 11, robotMaterial = 16, robotHitpoints = 4, robotBirthdate = 11, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 14 29 55, newMaterial = -6, newMemory = InternalState (fromList [])})],
            [(PositionedRobot ((-12,45), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 61 19 106, robotMaterial = 29, robotHitpoints = 40, robotBirthdate = 62, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 35 173 225, newMaterial = 5, newMemory = InternalState (fromList [])}),(PositionedRobot ((-11,45), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 198 148 144, robotMaterial = 6, robotHitpoints = 34, robotBirthdate = 34, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W)],
            [(PositionedRobot ((-32,-34), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 154 156 210, robotMaterial = 9, robotHitpoints = 20, robotBirthdate = 37, robotMemory = InternalState (fromList []), robotMessages = []}),Noop),(PositionedRobot ((-34,-32), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 151 115 192, robotMaterial = 41, robotHitpoints = 30, robotBirthdate = 20, robotMemory = InternalState (fromList []), robotMessages = []}),Fire FireAction {fireDirection = E, materialExpended = -37})],
            [(PositionedRobot ((-1,-2), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 140 27 170, robotMaterial = 11, robotHitpoints = 46, robotBirthdate = 37, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn N),(PositionedRobot ((-3,-1), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 234 132 136, robotMaterial = 42, robotHitpoints = 36, robotBirthdate = 26, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn S)],
            [(PositionedRobot ((33,2), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 18 75 15, robotMaterial = 19, robotHitpoints = 0, robotBirthdate = 10, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((34,2), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 227 112 174, robotMaterial = 4, robotHitpoints = 22, robotBirthdate = 33, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W),(PositionedRobot ((34,1), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 130 50 122, robotMaterial = 31, robotHitpoints = 8, robotBirthdate = 11, robotMemory = InternalState (fromList []), robotMessages = []}),Dig)],
            [(PositionedRobot ((0,-2), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 1 0 0, robotMaterial = 3, robotHitpoints = 4, robotBirthdate = 4, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 2 1 2, newMaterial = 2, newMemory = InternalState (fromList [])}),(PositionedRobot ((0,-1), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 2 2, robotMaterial = 3, robotHitpoints = 2, robotBirthdate = 3, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W)],
            [(PositionedRobot ((-50,1), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 66 168 195, robotMaterial = 63, robotHitpoints = 63, robotBirthdate = 41, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 159 139 144, newMaterial = 41, newMemory = InternalState (fromList [])}),(PositionedRobot ((-50,2), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 195 63, robotMaterial = 61, robotHitpoints = 3, robotBirthdate = 9, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn N)],
            [(PositionedRobot ((-1,8), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 0 2 3, robotMaterial = 3, robotHitpoints = 4, robotBirthdate = 4, robotMemory = InternalState (fromList []), robotMessages = []}),Fire FireAction {fireDirection = W, materialExpended = -1}),(PositionedRobot ((-1,7), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 2 1, robotMaterial = 4, robotHitpoints = 5, robotBirthdate = 5, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn S),(PositionedRobot ((-2,7), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 7 7 8, robotMaterial = 5, robotHitpoints = 2, robotBirthdate = 8, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 6 7 7, newMaterial = -4, newMemory = InternalState (fromList [])})],
            [(PositionedRobot ((-3,19), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 160 148 172, robotMaterial = 36, robotHitpoints = 21, robotBirthdate = 14, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn S),(PositionedRobot ((-2,19), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 36 49 200, robotMaterial = 25, robotHitpoints = 17, robotBirthdate = 1, robotMemory = InternalState (fromList []), robotMessages = []}),Noop),(PositionedRobot ((-3,18), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 43 202 119, robotMaterial = 24, robotHitpoints = 26, robotBirthdate = 8, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E)],
            [(PositionedRobot ((-12,45), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 61 19 106, robotMaterial = 29, robotHitpoints = 40, robotBirthdate = 62, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 35 173 225, newMaterial = 5, newMemory = InternalState (fromList [])}),(PositionedRobot ((-11,45), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 198 148 144, robotMaterial = 6, robotHitpoints = 34, robotBirthdate = 34, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W)],
            [(PositionedRobot ((9,21), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 400, robotHitpoints = 100, robotBirthdate = 2, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((21,9), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 400, robotHitpoints = 100, robotBirthdate = 2, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((9,20), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 996, robotHitpoints = 100, robotBirthdate = 19, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn N),(PositionedRobot ((20,9), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 996, robotHitpoints = 100, robotBirthdate = 19, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn N),(PositionedRobot ((21,10), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 238, robotHitpoints = 100, robotBirthdate = 0, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 255 0 0, newMaterial = 79, newMemory = InternalState (fromList [])}),(PositionedRobot ((10,21), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 238, robotHitpoints = 100, robotBirthdate = 0, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 0 0 128, newMaterial = 79, newMemory = InternalState (fromList [])}),(PositionedRobot ((19,8), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 999, robotHitpoints = 100, robotBirthdate = 26, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((8,19), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 999, robotHitpoints = 100, robotBirthdate = 26, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((7,19), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 408, robotHitpoints = 100, robotBirthdate = 16, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((19,7), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 408, robotHitpoints = 100, robotBirthdate = 16, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E)],
            [(PositionedRobot ((1,3), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 635, robotHitpoints = 100, robotBirthdate = 1, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((3,1), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 635, robotHitpoints = 100, robotBirthdate = 1, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((2,3), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 0 0 128, robotMaterial = 636, robotHitpoints = 100, robotBirthdate = 3, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E),(PositionedRobot ((3,2), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 255 0 0, robotMaterial = 636, robotHitpoints = 100, robotBirthdate = 3, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn E)],
            [(PositionedRobot ((-52,0), Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance $ PixelRGB8 94 250 23, robotMaterial = 35, robotHitpoints = 42, robotBirthdate = 48, robotMemory = InternalState (fromList []), robotMessages = []}),Spawn SpawnAction {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance $ PixelRGB8 42 190 236, newMaterial = -68, newMemory = InternalState (fromList [])}),(PositionedRobot ((-52,-2), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 242 34 8, robotMaterial = 30, robotHitpoints = 16, robotBirthdate = 39, robotMemory = InternalState (fromList []), robotMessages = []}),Noop),(PositionedRobot ((-51,0), Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance $ PixelRGB8 174 233 27, robotMaterial = 13, robotHitpoints = 38, robotBirthdate = 43, robotMemory = InternalState (fromList []), robotMessages = []}),MoveIn W)]
        ]
    where
    createCase :: Int -> [RobotAndAction] -> Test
    createCase n cas
            = TestCase $ assertBool
                ("MCR Edge Case " ++ show n
                    ++ "; Result = " ++ show b ++ "\n\t"
                    ++ show cas)
                (b == TRSuccess)
        where b = individualMCRChecks Nothing cas

assertTeamsSymmetric :: Board -> Test
assertTeamsSymmetric b
        = toTestCase $ teamSymmetric $ listOfRobots b


assertBoardSymmetry :: Board -> Test
assertBoardSymmetry b = TestList $ liftM2 symmetric [0.. boardSize b - 1] [0..boardSize b - 1]
    where
    symmetric x y = case u of
            Nothing -> TestCase $ assertBool "" True
            (Just value) -> TestCase $ assertBool
                ("The coordinates at " ++ show ((x, y), (y, x))++ " do not match") value
        where
        u = do
            (GameSpot regular _) <- b !!! (x, y)
            (GameSpot other _) <- b !!! (y, x)
            return $ regular == other

checks :: IO Bool
checks = all isSuccess <$> sequence (
        toolsChecks ++
        baseChecks ++
        robActChecks ++
        mcrChecks ++
        boardChecks ++
        if False then randomChecks else [putStrLn "Disabled cRandom checks (for now)" >> return (Success 0 [] "")])
