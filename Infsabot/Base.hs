module Infsabot.Base(
		Direction(N,E,W,S),
		Team(A,B),
		BoardSpot(SpotEmpty, SpotMaterial),
		Offset(Offset),
		InternalState,
		RobotAppearance(RobotAppearance), robotColor,
		SeenSpot(SeenSpot),
	) where

import qualified Data.Map as M
import Codec.Picture (PixelRGB8)

-- Represents one of the 4 potential directions
data Direction = N | E | W | S

-- Represents a Team. Currently, there are only two teams.
data Team = A | B

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
}

-- Represents a Spot on the Board as seen by a robot.
-- This contains a Board Spot, which the Robot can always see, contains a robot's appearance iff there is a robot at that spot.
data SeenSpot = SeenSpot BoardSpot (Maybe RobotAppearance)
