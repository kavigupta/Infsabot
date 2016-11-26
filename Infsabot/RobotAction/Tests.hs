{-# Language TemplateHaskell #-}
{-# Language OverlappingInstances #-}
{-# Language FlexibleInstances #-}
module Infsabot.RobotAction.Tests(
        robActChecks
    ) where

import Infsabot.RobotAction.Logic
import Infsabot.Tools.Interface
import Infsabot.Parameters
import Data.Map(Map, fromList)

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck hiding (shuffle)

import Infsabot.Base.Tests()

robActChecks :: [IO Result]
robActChecks = [
        putStrLn "Action cost positive" >> doChecks 4 propActionCostPositive
    ]

propActionCostPositive :: Parameters -> RobotAction -> Bool
propActionCostPositive p ra = actionCost p ra >= 0

instance CoArbitrary KnownState where
    coarbitrary _ = id

instance Arbitrary (Map String String) where
    arbitrary = return $ fromList []

$( derive makeArbitrary ''RobotAction )
$( derive makeArbitrary ''SpawnAction )
$( derive makeArbitrary ''FireAction )
$( derive makeArbitrary ''SendAction )
