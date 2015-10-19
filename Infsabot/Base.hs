module Infsabot.Base(
		BoardSpot(SpotEmpty, SpotMaterial),
		Direction(N,E,W,S),
		Team(A,B),
		Offset(Offset),
		InternalState
	) where

import qualified Data.Map as M

data BoardSpot = SpotEmpty | SpotMaterial
	deriving (Eq, Show)

-- Represents one of the 4 potential directions
data Direction = N | E | W | S

-- Represents a Team. Currently, there are only two teams.
data Team = A | B

-- Represents an offset from the original position.
newtype Offset = Offset Int

-- The robot's internal state. This is represented by a Stringly-typed Map
type InternalState = M.Map String String
