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
import qualified Data.Map as M

import Data.DeriveTH(derive, makeArbitrary)
--import Debug.Trace
import Test.QuickCheck hiding (shuffle)

checks :: IO ()
checks =
    do
        putStrLn "checking"
        quickCheck propOrderIndependence

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
        msgs <- arbitrary
        return $ Robot {
            robotProgram = basicProgram team,
            robotTeam = team,
            robotAppearance = appearance,
            robotMaterial = abs mat,
            robotHitpoints = abs hp,
            robotBirthdate = abs date,
            robotMemory = state,
            robotMessages = msgs
}

instance CoArbitrary KnownState where
    coarbitrary _ = id

instance Arbitrary (M.Map String String) where
    arbitrary = contents >>= (return . M.fromList)
        where contents = arbitrary :: Gen [(String, String)]
