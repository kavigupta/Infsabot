{-# LANGUAGE TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Infsabot.Board.Tests(
    assertRobotSourcesAgree, boardChecks
    ) where

import Infsabot.Board.Logic
import qualified Data.RandomAccessList as DRal

import Test.QuickCheck hiding (shuffle)
import qualified Data.Map as Map
import Infsabot.Robot.Interface

import Infsabot.Base.Interface

import Control.Monad(liftM, liftM2)
import Test.HUnit

import Infsabot.Tools.Interface
import Data.DeriveTH(derive, makeArbitrary)
import Data.Maybe

import Infsabot.RobotAction.Tests()

type RAL = DRal.RandomAccessList

boardChecks :: [IO Result]
boardChecks
    = [
            putStrLn "Get (Set) Robots" >> doChecks 1 propSetGetRobots,
            putStrLn "Get (Set) Spots" >> doChecks 1 propSetGetBoardSpots
        ]

--- setting a robot means that you can get it out
propSetGetRobots :: Board -> (Int, Int) -> Robot -> Bool
propSetGetRobots b xy' rob
    = robotAt (setRobot xy (Just rob) b) xy == Just rob
        where xy = coerceIntoBoard b xy'

--- setting a robot means that you can get it out
propSetGetBoardSpots :: Board -> (Int, Int) -> BoardSpot -> Bool
propSetGetBoardSpots b xy' spot
    = spot == spot'
        where
        xy = coerceIntoBoard b xy'
        (GameSpot spot' _) = fromJust $ updateSpot xy spot b !!! xy

coerceIntoBoard :: Board -> (Int, Int) -> (Int, Int)
coerceIntoBoard b (x, y) = (x `mod` boardSize b, y `mod` boardSize b)

robotsOnBoard :: Board -> [PositionedRobot]
robotsOnBoard b = concat $ zipWith (curry robotExtractor) coordinates $ map (robotAt b) coordinates
    where
    coordinates :: [(Int, Int)]
    coordinates = liftM2 (,) [0..boardSize b - 1] [0..boardSize b - 1]
    robotExtractor :: ((Int, Int), Maybe Robot) -> [PositionedRobot]
    robotExtractor (_, Nothing) = []
    robotExtractor ((x, y), Just rob) = [PositionedRobot ((x, y), rob)]

assertRobotSourcesAgree :: Board -> Test
assertRobotSourcesAgree b
    = TestCase $
        assertBool "Robot sources boardRobots and boardContents agree" $
        sameElements (map PositionedRobot $ Map.toList $ boardRobots b) (robotsOnBoard b)

instance Arbitrary Board where
    arbitrary = do
            size <- choose (1, 30)
            time <- choose (1, 100)
            contents <- arbitraryBoard size
            let robots = robotsOnBoard Board {boardSize = size, boardTime = time, boardRobots = Map.fromList [], boardContents = contents}
            return Board {boardSize = size, boardTime = time, boardRobots = Map.fromList $ map (\(PositionedRobot x) -> x) robots, boardContents = contents}
        where
        arbitraryBoard :: Int -> Gen (RAL (RAL GameSpot))
        arbitraryBoard size = liftM DRal.fromList $ arbitraryBoardL size
            where
            arbitraryBoardL :: Int -> Gen [RAL GameSpot]
            arbitraryBoardL 0 = return []
            arbitraryBoardL n =
                    do
                        first <- arbitraryRow
                        rest <- arbitraryBoardL (n - 1)
                        return $ first : rest
            arbitraryRow :: Gen (RAL GameSpot)
            arbitraryRow = do
                inf2 <- infiniteList
                return $ DRal.fromList $ take size inf2

$( derive makeArbitrary ''GameSpot )
$( derive makeArbitrary ''BoardSpot )
