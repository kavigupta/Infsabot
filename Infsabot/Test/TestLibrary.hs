{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Infsabot.Test.TestLibrary(
        TestResult(..),
        TeamedObject, positionOf, teamOf,
        TeamedComparable, areSymm, symmetricOf,
        toTestCase, constructTest,
        effectivePosition, teamSymmetric
    ) where

import Infsabot.Base.Interface
import Test.HUnit
import Data.List
import Data.Tuple(swap)
import Infsabot.Robot
import Data.Monoid
import Infsabot.RobotAction
import Infsabot.Debug
import Data.Function

data TestResult a = TRSuccess | TRFailure a deriving (Show, Eq)

instance Functor TestResult where
    fmap _ TRSuccess = TRSuccess
    fmap f (TRFailure x) = TRFailure (f x)

instance Monoid (TestResult a) where
    mempty = TRSuccess
    TRSuccess `mappend` x = x
    (TRFailure x) `mappend` _ = (TRFailure x)

class TeamedObject a where
    positionOf :: a -> (Int, Int)
    teamOf :: a -> Team

class TeamedComparable a where
    areSymm :: (a, a) -> TestResult String
    symmetricOf :: a -> a

instance TeamedObject PositionedRobot where
    positionOf (PositionedRobot (pos, _)) = pos
    teamOf (PositionedRobot (_, rob)) = robotTeam rob

instance TeamedComparable Robot where
    areSymm (rob1, rob2) = mconcat $ zipWith constructTest results assertLabels
        where
        results :: [Bool]
        results = map (\u -> u rob1 rob2) assertions
        assertions :: [Robot -> Robot -> Bool]
        assertions = [(==) `on` robotMaterial,
                        (==) `on` robotHitpoints,
                        (==) `on` robotBirthdate,
                        (==) `on` robotMemory,
                        (==) `on` robotMessages]
        assertLabels = map (\x -> "Different " ++ x ++ " in the robots "
            ++ show rob1
            ++ " and " ++ show rob2) [
                "material",
                "hitpoints",
                "birthdate",
                "memory",
                "messages"
            ]
    symmetricOf rob = rob {robotTeam = symmetricOf $ robotTeam rob}

instance TeamedComparable PositionedRobot where
    areSymm (PositionedRobot ((x1, y1), rob1), (PositionedRobot ((x2, y2), rob2)))
        | x1 /= y2 || y1 /= x2
            = TRFailure
                $  "The Team A robot at "
                ++ show (x1, y1)
                ++ " appears to have no corresponding B robot"
        | otherwise
            = areSymm (rob1, rob2)
    symmetricOf (PositionedRobot ((x, y), r)) = (PositionedRobot ((symmetricOf y, symmetricOf x), symmetricOf r))

instance TeamedComparable Team where
    areSymm (a, b)
        | a == b    = TRFailure "Teams must be of different types"
        | otherwise = TRSuccess
    symmetricOf A = B
    symmetricOf B = A

instance TeamedComparable RDirection where
    areSymm (x, y)
        | x == y    = TRSuccess
        | otherwise
            = TRFailure $ "The directions "
                ++ show x
                ++ " and "
                ++ show y
                ++ " do not correspond."
    symmetricOf x = x
    --symmetricOf x = oppositeDirection . symmetricOf . oppositeDirection $ x

scalarSymm :: (Eq a, Show a) => (a, a) -> TestResult String
scalarSymm (x, y)
    | x == y    = TRSuccess
    | otherwise
        = TRFailure $ "The quantities "
            ++ show x ++ " and "
            ++ show y ++ " do not correspond."

instance TeamedComparable Int where
    areSymm = scalarSymm
    symmetricOf = id

instance TeamedComparable String where
    areSymm = scalarSymm
    symmetricOf = id

instance TeamedComparable InternalState where
    areSymm = scalarSymm
    symmetricOf = id


instance TeamedComparable RobotProgram where
    areSymm = const TRSuccess
    symmetricOf = id


instance TeamedComparable RobotAppearance where
    areSymm = const TRSuccess
    symmetricOf = id

instance TeamedComparable RobotAction where
    areSymm (x, y) = fmap
        (("The actions "
            ++ show x
            ++ " and "
            ++ show y
            ++ " do not correspond;")
            ++) $ aS (x, y)
        where
        aS (Noop, Noop) = TRSuccess
        aS (Die, Die) = TRSuccess
        aS (Dig, Dig) = TRSuccess
        aS (MoveIn a, MoveIn b) = areSymm (a, b)
        aS (Fire (FireAction mat1 dir1), Fire  (FireAction mat2 dir2))
            = areSymm(mat1, mat2) `mappend` areSymm (dir1, dir2)
        aS (Send (SendAction str1 dir1), Send (SendAction str2 dir2))
            = areSymm (str1, str2) `mappend` areSymm (dir1, dir2)
        aS (Spawn s1, Spawn s2)
            = areSymm (newDirection s1, newDirection s2) `mappend`
                areSymm(newMaterial s1, newMaterial s2) `mappend`
                areSymm (newMemory s1, newMemory s2)
        aS _ = TRFailure ""
    symmetricOf Noop = Noop
    symmetricOf Die = Die
    symmetricOf Dig = Dig
    symmetricOf (MoveIn a) = MoveIn $ symmetricOf a
    symmetricOf (Fire (FireAction a b)) = Fire $ FireAction (symmetricOf a) (symmetricOf b)
    symmetricOf (Send (SendAction a b)) = Send $ SendAction (symmetricOf a) (symmetricOf b)
    symmetricOf (Spawn (SpawnAction a b c d e))
        = Spawn $ SpawnAction
            (symmetricOf a)
            (symmetricOf b)
            (symmetricOf c)
            (symmetricOf d)
            (symmetricOf e)

instance TeamedObject RobotAndAction where
    positionOf = positionOf . fst
    teamOf = teamOf . fst

instance TeamedComparable RobotAndAction where
    areSymm ((xyr1, act1), (xyr2, act2)) = areSymm (xyr1, xyr2) `mappend` areSymm (act1, act2)
    symmetricOf (a, b) = (symmetricOf a, symmetricOf b)


toTestCase :: TestResult String -> Test
toTestCase TRSuccess = TestCase $ assertBool "" True
toTestCase (TRFailure x) = TestCase $ assertBool x False

constructTest :: Bool -> a -> TestResult a
constructTest True = const TRSuccess
constructTest False = TRFailure

effectivePosition :: (TeamedObject a) => a -> (Int, Int)
effectivePosition x
    | teamOf x == A         = positionOf x
    | otherwise             = swap $ positionOf x

teamSymmetric :: (TeamedObject a, TeamedComparable a, Show a) => [a] -> TestResult String
teamSymmetric raas
    | trace ("sortedAsBs " ++ show sortedAsBs)  False = undefined
    | otherwise = mconcat (map areSymm sortedAsBs) `mappend` equalLength
    where
    (as, bs) = partition (\x -> teamOf x == A) raas
    equalLength = constructTest
        (length as == length bs)
        ("Inequal numbers of robots\n\t"
            ++ show as
            ++ "\n\t"
            ++ show bs)
    sortedAsBs
        = zip
            (sortBy (compare `on` positionOf) as)
            (sortBy (compare `on` swap . positionOf) bs)
