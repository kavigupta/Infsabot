module Infsabot.Base(BoardSpot(SpotEmpty, SpotMaterial)) where

data BoardSpot = SpotEmpty | SpotMaterial
	deriving (Eq, Show)
