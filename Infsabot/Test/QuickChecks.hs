{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}
module Infsabot.Test.QuickChecks () where

import Infsabot.RobotAction
import Infsabot.Robot
import Infsabot.Parameters
import Infsabot.Board.Tests()

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck hiding (shuffle)

instance CoArbitrary KnownState where
    coarbitrary _ = id

$( derive makeArbitrary ''RobotAction )
$( derive makeArbitrary ''SpawnAction )
$( derive makeArbitrary ''FireAction )
$( derive makeArbitrary ''SendAction )
$( derive makeArbitrary ''PositionedRobot )
$( derive makeArbitrary ''LinearF )
$( derive makeArbitrary ''Parameters )
