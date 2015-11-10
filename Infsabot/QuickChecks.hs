{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Infsabot.QuickChecks (checks, propOrderIndependence) where

import Infsabot.Tools(shuffle)

import Data.List((\\))

import Infsabot.Base
import Codec.Picture
import Infsabot.RobotAction
import Infsabot.RobotStrategy
import Infsabot.Robot
import Infsabot.MoveConflictResolution
import Data.Map(fromList, Map)

import Data.DeriveTH(derive, makeArbitrary)
--import Debug.Trace
import Test.QuickCheck hiding (shuffle)

checks :: IO ()
checks =
    do
        putStrLn "checking"
        quickCheck propConflictOrderIndependence
        quickCheck propOrderIndependence

propConflictOrderIndependence :: (FinalLocations, FinalLocations) -> Bool
propConflictOrderIndependence (x, y) = a == c && b == d
    where
    (Remove a b) = doConflict x y
    (Remove d c) = doConflict y x

propOrderIndependence :: [RobotAndAction] -> Int -> Bool
propOrderIndependence xs seed = length originalOut == length shuffleOut
        && (null $ originalOut \\ shuffleOut)
    where
    originalOut = removeConflicting xs
    shuffleOut = removeConflicting $ shuffle seed xs

$( derive makeArbitrary ''RDirection )
$( derive makeArbitrary ''RobotAction )
$( derive makeArbitrary ''Team )
$( derive makeArbitrary ''RobotAppearance )
$( derive makeArbitrary ''PixelRGB8 )

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
