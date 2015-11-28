module Infsabot.Base(
                RDirection(N,E,W,S),
                        oppositeDirection,
                Team(A,B),
                BoardSpot(SpotEmpty, SpotMaterial),
                        applyDirection, limitedOffset,
                InternalState,
                RobotAppearance(RobotAppearance), robotColor,
                SeenSpot(SeenSpot),
        ) where

import Data.Tuple(swap)
import Data.Map(Map)
import Codec.Picture (PixelRGB8)

-- Represents one of the 4 potential directions, relative to the Robot itself
data RDirection = N | E | W | S deriving (Show, Eq)

-- Represents a Team
data Team = A | B deriving (Show, Eq)

-- A spot on the Board. This is either empty or contains material.
data BoardSpot = SpotEmpty | SpotMaterial
        deriving (Eq, Show)

-- Represents an offset from the original position.
newtype Offset = Offset Int

-- The robot's internal state. This is represented by a Stringly-typed Map
type InternalState = Map String String

-- The robot's appearance. Currently just contains a color.
data RobotAppearance = RobotAppearance {
        robotColor :: PixelRGB8
} deriving (Show, Eq)

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see,
--      contains a robot's appearance iff there is a robot at that spot.
data SeenSpot = SeenSpot BoardSpot (Maybe RobotAppearance)


{-
    Offsets the given coordinate by the given directions, if the length of the overall
    offset is less than the given length
-}
limitedOffset :: Team -> Int -> [RDirection] -> (Int, Int) -> Maybe (Int, Int)
limitedOffset team len directs to
        | withinRange   = Just $ applyOffset offs to
        | otherwise     = Nothing
    where
    withinRange = squareNorm offs <= len * len
    offs = overallOffset team directs


applyDirection :: Team -> RDirection -> (Int, Int) -> (Int, Int)
applyDirection team dir = applyOffset (getOffset team dir)

overallOffset :: Team -> [RDirection] -> (Offset, Offset)
overallOffset team directs = foldr (addOffset) (Offset 0, Offset 0) $ map (getOffset team) directs

-- Gets the coordinate pair of offsets representing the given Team's understanding of the given direction
getOffset :: Team -> RDirection -> (Offset, Offset)
getOffset B N = (Offset 0, Offset (-1))
getOffset B E = (Offset 1, Offset 0)
getOffset B dir = negateOff . getOffset B . oppositeDirection $ dir
getOffset A dir = swap $ getOffset B dir

applyOffset :: (Offset, Offset) -> (Int, Int) -> (Int, Int)
applyOffset (Offset offx, Offset offy) (x, y) = (x + offx, y + offy)

negateOff :: (Offset, Offset) -> (Offset, Offset)
negateOff (Offset offx, Offset offy) = (Offset (-offx), Offset (-offy))

addOffset :: (Offset, Offset) -> (Offset, Offset) -> (Offset, Offset)
addOffset         (Offset offx1, Offset offy1)
                        (Offset offx2, Offset offy2)
                = (Offset (offx1 + offx2), Offset (offy1 + offy2))

oppositeDirection :: RDirection -> RDirection
oppositeDirection N = S
oppositeDirection S = N
oppositeDirection E = W
oppositeDirection W = E

squareNorm :: (Offset, Offset) -> Int
squareNorm (Offset x, Offset y) = x * x + y * y
