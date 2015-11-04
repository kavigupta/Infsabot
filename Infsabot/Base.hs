module Infsabot.Base(
		RDirection(N,E,W,S),
			oppositeDirection,
		Team(A,B),
		BoardSpot(SpotEmpty, SpotMaterial),
		Offset(Offset),
			getOffset, applyOffset, addOffset, squareNorm, asSeen,
		InternalState,
		RobotAppearance(RobotAppearance), robotColor,
		SeenSpot(SeenSpot),
		unpack
	) where

import qualified Data.Map as M
import Codec.Picture (PixelRGB8)

-- Represents one of the 4 potential directions
data RDirection = N | E | W | S deriving (Show, Eq)

-- Represents a Team. Currently, there are only two teams.
data Team = A | B deriving (Show, Eq)

-- A spot on the Board. This is either empty or contains material.
data BoardSpot = SpotEmpty | SpotMaterial
	deriving (Eq, Show)

-- Represents an offset from the original position.
newtype Offset = Offset Int

-- The robot's internal state. This is represented by a Stringly-typed Map
type InternalState = M.Map String String

-- The robot's appearance. Currently just contains a color.
data RobotAppearance = RobotAppearance {
	robotColor :: PixelRGB8
} deriving (Show)

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data SeenSpot = SeenSpot BoardSpot (Maybe RobotAppearance)

getOffset :: Team -> RDirection -> (Offset, Offset)
getOffset B N = (Offset 0, Offset (-1))
getOffset B E = (Offset 1, Offset 0)
getOffset B W = (Offset (-1), Offset 0)
getOffset B S = (Offset 0, Offset 1)
getOffset A dir = getOffset B $ oppositeDirection dir

applyOffset :: (Offset, Offset) -> (Int, Int) -> (Int, Int)
applyOffset (Offset offx, Offset offy) (x, y) = (x + offx, y + offy)

addOffset :: (Offset, Offset) -> (Offset, Offset) -> (Offset, Offset)
addOffset 	(Offset offx1, Offset offy1)
			(Offset offx2, Offset offy2)
		= (Offset (offx1 + offx2), Offset (offy1 + offy2))

oppositeDirection :: RDirection -> RDirection
oppositeDirection N = S
oppositeDirection S = N
oppositeDirection E = W
oppositeDirection W = E

squareNorm :: (Offset, Offset) -> Int
squareNorm (Offset x, Offset y) = x * x + y * y

unpack :: (Maybe a) -> a
unpack Nothing = error "logic error"
unpack (Just x) = x

asSeen :: Team -> (Offset, Offset) -> (Offset, Offset)
asSeen A xy = xy
asSeen B (Offset x, Offset y) = (Offset (-x), Offset (-y))
