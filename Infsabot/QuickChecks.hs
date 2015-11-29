{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Infsabot.QuickChecks (checks, propConflictsResolved, propOrderIndependence) where

import Infsabot.Tools

import Infsabot.Base
import Codec.Picture
import Infsabot.RobotAction
import Infsabot.RobotStrategy
import Infsabot.Robot
import Infsabot.Board
import Infsabot.MoveConflictResolution
import Infsabot.Parameters
import Infsabot.TestLibrary
import Infsabot.GamePlay
import Data.Function(on)
import Data.List(nubBy)
import Data.Map(fromList, Map, toList)
import Control.Monad(forM_, liftM)

import qualified Data.RandomAccessList as DRal

import Data.DeriveTH(derive, makeArbitrary)
--import Debug.Trace
import Test.QuickCheck hiding (shuffle)

type RAL = DRal.RandomAccessList

checkCount :: Int
checkCount = 4000

doChecks :: (Testable prop) => Int -> prop -> IO ()
doChecks n = quickCheckWith $ stdArgs { maxSuccess = n }

checks :: IO ()
checks =
    do
        putStrLn "checking"
        forM_ mcrChecks $ \check -> check
        quickCheck rListBoardCorr

-- GamePlay checks follow

rListBoardCorr :: Parameters -> Board -> Bool
rListBoardCorr p b = sameElements (map PositionedRobot $ toList $ boardRobots nextb) (robotsOnBoard nextb)
    where nextb = boards p b !! 1

-- Move Conflict Resolution tests Follow

mcrChecks :: [IO ()]
mcrChecks = [
    putStrLn "Conflict Order Independence" >> doChecks (5 * checkCount) propConflictOrderIndependence,
    putStrLn "No Change In Length" >> doChecks checkCount propNoChangeInLength,
    putStrLn "Symmetery Preserving" >> doChecks (5 * checkCount) propSymmeteryPreserving,
    putStrLn "Conflicts Resolved" >> doChecks (50 * checkCount) (\x -> uncurry (==>) $ propConflictsResolved x)]

propSymmeteryPreserving :: [RobotAndAction] -> Bool
propSymmeteryPreserving raas
    = teamSymmetric
            (removeConflicting $ makeSymmetric raas) == TRSuccess

propConflictOrderIndependence :: (RAAFL, RAAFL) -> Property
propConflictOrderIndependence (x, y) = location x /= location y
        ==> a == c && b == d
    where
    (a, b) = conflictsBetween x y
    (d, c) = conflictsBetween y x

propNoChangeInLength :: [RobotAndAction] -> Bool
propNoChangeInLength r = length raas == length (removeConflicting raas)
    where raas = nubBy ((==) `on` positionOf) r

location :: RAAFL -> (Int, Int)
location = positionOf . fst

$( derive makeArbitrary ''RDirection )
$( derive makeArbitrary ''SpawnAction )
$( derive makeArbitrary ''FireAction )
$( derive makeArbitrary ''SendAction )
$( derive makeArbitrary ''RobotAction )
$( derive makeArbitrary ''PositionedRobot )
$( derive makeArbitrary ''Team )
$( derive makeArbitrary ''RobotAppearance )
$( derive makeArbitrary ''PixelRGB8 )
$( derive makeArbitrary ''GameSpot )
$( derive makeArbitrary ''FinalLocs )
$( derive makeArbitrary ''BoardSpot )
$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )

instance Arbitrary Board where
    arbitrary = do
            size <- choose (1, 30)
            time <- choose (1, 100)
            contents <- arbitraryBoard size
            let robots = robotsOnBoard $ Board {boardSize = size, boardTime = time, boardRobots = fromList [], boardContents = contents}
            return $ Board {boardSize = size, boardTime = time, boardRobots = fromList $ map (\(PositionedRobot x) -> x) robots, boardContents = contents}
        where
        arbitraryBoard :: Int -> Gen (RAL (RAL GameSpot))
        arbitraryBoard size = liftM DRal.fromList $ arbitraryBoardL size
            where
            arbitraryBoardL :: Int -> Gen [(RAL GameSpot)]
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

instance Arbitrary Robot where
    arbitrary = do
        mat <- arbitrary
        team <- arbitrary
        appearance <- arbitrary
        hp <- arbitrary
        date <- arbitrary
        state <- arbitrary
        return $ Robot {
            robotProgram = basicProgram team,
            robotTeam = team,
            robotAppearance = appearance,
            robotMaterial = abs mat,
            robotHitpoints = abs hp,
            robotBirthdate = abs date,
            robotMemory = state,
            robotMessages = []
}

instance CoArbitrary KnownState where
    coarbitrary _ = id

instance Arbitrary (Map String String) where
    arbitrary = return (fromList [])
