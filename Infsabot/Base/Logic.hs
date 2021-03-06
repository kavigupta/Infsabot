{-# LANGUAGE TemplateHaskell #-}
module Infsabot.Base.Logic(
                RDirection(N,E,W,S),
                        oppositeDirection,
                Team(A,B),
                BoardSpot(SpotEmpty, SpotMaterial),
                        applyDirection, limitedOffset,
                InternalState(InternalState),
                RobotAppearance(RobotAppearance),
                SeenSpot(SeenSpot),
                colorOf,
                get, insert
        ) where

import Data.Tuple(swap)
import qualified Data.Map as M
import Codec.Picture (PixelRGB8(PixelRGB8))
import Test.QuickCheck hiding (shuffle)

import Data.DeriveTH(derive, makeArbitrary)

-- | Represents one of the 4 potential directions, relative to the Robot itself
data RDirection = N | E | W | S deriving (Show, Eq)

-- | Represents a Team
data Team = A | B deriving (Show, Eq)

-- | A spot on the Board. This is either empty or contains material.
data BoardSpot = SpotEmpty | SpotMaterial
        deriving (Eq, Show)

-- | Represents an offset from the original position.
newtype Offset = Offset (Int, Int)

-- | The robot's internal state. This is represented by a Stringly-typed Map
newtype InternalState = InternalState (M.Map String String) deriving (Show, Eq)

get :: InternalState -> String -> Maybe String
get (InternalState x) = flip M.lookup x

insert :: InternalState -> String -> String -> InternalState
insert (InternalState x) k v = InternalState $ M.insert k v x

-- | The robot's appearance. Currently just contains a color.
data RobotAppearance = RobotAppearance PixelRGB8 deriving (Show, Eq)

{- | Represents a Spot on the Board as seen by a robot.
    This contains a Board Spot, which the Robot can always see,
    contains a robot's appearance iff there is a robot at that spot.
-}
data SeenSpot = SeenSpot BoardSpot (Maybe RobotAppearance)


{- |
    Offsets the given coordinate by the given directions, if the length of the overall
    offset is less than the given length
-}
limitedOffset ::
    Team -- ^ The team to use in determining the offsets of the path
     -> Int -- ^ The maximum allowable length of the path
     -> [RDirection] -- ^ The path to offset the original by
     -> (Int, Int) -- ^ The original location
     -> Maybe (Int, Int)
limitedOffset team len directs to
        | withinRange   = Just $ applyOffset offs to
        | otherwise     = Nothing
    where
    withinRange = squareNorm offs <= len * len
    offs = overallOffset team directs

{- |
    Apply the given direction to the given coordinate pair
-}
applyDirection ::
    Team -- ^ The team to use in analyzing the direction
    -> RDirection -- ^ The direction to travel in
    -> (Int, Int) -- ^ The original position
    -> (Int, Int) -- ^ The offset position
applyDirection team dir = applyOffset (getOffset team dir)

-- | The overall offset of the given list of directions, which is the vector sum of its components
overallOffset :: Team -> [RDirection] -> Offset
overallOffset team = foldr (addOffset . getOffset team) (Offset (0, 0))

-- | Gets the coordinate pair of offsets representing the given Team's understanding of the given direction
getOffset :: Team -> RDirection -> Offset
getOffset B N = Offset (0, -1)
getOffset B E = Offset (1, 0)
getOffset B dir = negateOff . getOffset B . oppositeDirection $ dir
getOffset A dir = swapsset $ getOffset B dir
    where swapsset (Offset x) = Offset . swap $ x

-- | Applies the given offset to the given coordinate
applyOffset :: Offset -> (Int, Int) -> (Int, Int)
applyOffset (Offset (offx, offy)) (x, y) = (x + offx, y + offy)

-- | Negates the given offset
negateOff :: Offset -> Offset
negateOff (Offset (offx, offy)) = Offset (-offx, -offy)

-- | Adds the given two offsets
addOffset :: Offset -> Offset -> Offset
addOffset a (Offset b) = Offset $ applyOffset a b

-- | The opposite direction to the given direction
-- prop> oppositeDirection (oppositeDirection x) == x
oppositeDirection :: RDirection -> RDirection
oppositeDirection N = S
oppositeDirection S = N
oppositeDirection E = W
oppositeDirection W = E

squareNorm :: Offset -> Int
squareNorm (Offset (x, y)) = x * x + y * y

{-
    Represents objects that have colors.
-}
class Colored a where
    colorOf :: a -> PixelRGB8

instance Colored BoardSpot where
    colorOf SpotEmpty =  PixelRGB8 255 255 255
    colorOf SpotMaterial = PixelRGB8 128 128 128

instance Colored Team where
    colorOf A = PixelRGB8 255 0 0
    colorOf B = PixelRGB8 0 0 128

instance Colored RobotAppearance where
    colorOf (RobotAppearance x) = x

    -- Gets the display color of the given spot.
    -- If there is a robot, then the color is that of the robot
    -- Otherwise, the color is that of the underlying material
instance Colored SeenSpot where
    colorOf (SeenSpot _ (Just rob))             = colorOf rob
    colorOf (SeenSpot x Nothing)                = colorOf x

$( derive makeArbitrary ''RDirection )
