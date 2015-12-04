{-# Language TemplateHaskell #-}

module Infsabot.Robot.Tests() where

import Infsabot.Robot.Logic
import Infsabot.RobotStrategy

import Test.QuickCheck hiding (shuffle)
import Data.DeriveTH(derive, makeArbitrary)

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
$( derive makeArbitrary ''PositionedRobot )
