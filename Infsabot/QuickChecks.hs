{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Infsabot.QuickChecks () where

import Infsabot.Base.Interface
import Codec.Picture
import Infsabot.RobotAction
import Infsabot.RobotStrategy
import Infsabot.Robot
import Infsabot.Board
import Infsabot.Parameters
import Infsabot.TestLibrary
import Data.Map(fromList, Map)
import Control.Monad(liftM)

import qualified Data.RandomAccessList as DRal

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck hiding (shuffle)

type RAL = DRal.RandomAccessList

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

$( derive makeArbitrary ''RobotAction )
$( derive makeArbitrary ''SpawnAction )
$( derive makeArbitrary ''FireAction )
$( derive makeArbitrary ''SendAction )
$( derive makeArbitrary ''RobotAppearance )
$( derive makeArbitrary ''PositionedRobot )
$( derive makeArbitrary ''PixelRGB8 )
$( derive makeArbitrary ''GameSpot )
$( derive makeArbitrary ''BoardSpot )
$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )
