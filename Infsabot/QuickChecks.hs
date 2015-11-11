{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Infsabot.QuickChecks (checks) where

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
import Data.Map(fromList, Map)
import Control.Monad(forM_, liftM)

import qualified Data.RandomAccessList as DRal

import Data.DeriveTH(derive, makeArbitrary)
--import Debug.Trace
import Test.QuickCheck hiding (shuffle)

checks :: IO ()
checks =
    do
        putStrLn "checking"
        forM_ mcrChecks $ \check -> check
        quickCheck rListBoardCorr

-- GamePlay checks follow

rListBoardCorr :: Parameters -> Board -> Bool
rListBoardCorr p b = sameElements (boardRobots nextb) (robotsOnBoard nextb)
    where nextb = boards p b !! 1

-- Move Conflict Resolution tests Follow

mcrChecks :: [IO ()]
mcrChecks = [quickCheck propConflictOrderIndependence, quickCheck $ propOrderIndependence removeConflicting, quickCheck propConflictsResolved]

propConflictsResolved :: [RobotAndAction] -> Property
propConflictsResolved acts
    = allDifferent (map getLocation acts)
        ==> allDifferent finalLocs
    where
    finalLocs :: [(Int, Int)]
    finalLocs = concat $ map (map loc . finalLocations) $ removeConflicting acts
        where
        loc :: (Int, Int, Bool) -> (Int, Int)
        loc (x, y, _) = (x, y)
    getLocation :: RobotAndAction -> (Int, Int)
    getLocation ((x, y, _), _) = (x, y)

propConflictOrderIndependence :: (FinalLocations, FinalLocations) -> Bool
propConflictOrderIndependence (x, y) = a == c && b == d
    where
    (Remove a b) = doConflict x y
    (Remove d c) = doConflict y x

$( derive makeArbitrary ''RDirection )
$( derive makeArbitrary ''RobotAction )
$( derive makeArbitrary ''Team )
$( derive makeArbitrary ''RobotAppearance )
$( derive makeArbitrary ''PixelRGB8 )
$( derive makeArbitrary ''GameSpot )
$( derive makeArbitrary ''BoardSpot )
$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )

instance Arbitrary Board where
    arbitrary = do
            size <- choose (1, 30)
            time <- choose (1, 100)
            contents <- arbitraryBoard size
            let robots = robotsOnBoard $ Board {boardSize = size, boardTime = time, boardRobots = [], boardContents = contents}
            return $ Board {boardSize = size, boardTime = time, boardRobots = robots, boardContents = contents}
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
