module Infsabot.Base.Interface (
                RDirection(N,E,W,S),
                        oppositeDirection,
                Team(A,B),
                BoardSpot(SpotEmpty, SpotMaterial),
                        applyDirection, limitedOffset,
                InternalState(..),
                RobotAppearance(RobotAppearance),
                SeenSpot(SeenSpot),
                colorOf,
                baseChecks
        ) where

import Infsabot.Base.Logic
import Infsabot.Base.Tests
