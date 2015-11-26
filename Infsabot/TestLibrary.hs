{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Infsabot.TestLibrary(robotsOnBoard,
    propOrderIndependence,
    propConflictsResolved,
    areSymm, makeSymmetric, teamSymmetric,
    TestResult(..),
    constructTest, toTestCase) where

import Infsabot.Board
import Infsabot.Base
import Test.HUnit
import Data.List(nubBy, partition, sortBy)
import Data.Tuple(swap)
import Infsabot.Robot
import Data.Monoid
import Control.Monad
import Infsabot.Tools
import Infsabot.RobotAction
import Infsabot.Debug
import Data.Function
import Infsabot.MoveConflictResolution


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

instance TeamedObject (Int, Int, Robot) where
    positionOf (x, y, _) = (x, y)
    teamOf (_, _, rob) = robotTeam rob

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

instance TeamedComparable (Int, Int, Robot) where
    areSymm ((x1, y1, rob1), (x2, y2, rob2))
        | x1 /= y2 || y1 /= x2
            = TRFailure
                $  "The Team A robot at "
                ++ show (x1, y1)
                ++ " appears to have no corresponding B robot"
        | otherwise
            = areSymm (rob1, rob2)
    symmetricOf (x, y, r) = (symmetricOf y, symmetricOf x, symmetricOf r)

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
        aS (Fire dir1 mat1, Fire dir2 mat2)
            = areSymm(mat1, mat2) `mappend` areSymm (dir1, dir2)
        aS (SendMessage str1 dir1, SendMessage str2 dir2)
            = areSymm (str1, str2) `mappend` areSymm (dir1, dir2)
        aS (s1@(Spawn _ _ _ _ _), s2@(Spawn _ _ _ _ _))
            = areSymm (newDirection s1, newDirection s2) `mappend`
                areSymm(newMaterial s1, newMaterial s2) `mappend`
                areSymm (newMemory s1, newMemory s2)
        aS _ = TRFailure ""
    symmetricOf Noop = Noop
    symmetricOf Die = Die
    symmetricOf Dig = Dig
    symmetricOf (MoveIn a) = MoveIn $ symmetricOf a
    symmetricOf (Fire a b) = Fire (symmetricOf a) (symmetricOf b)
    symmetricOf (SendMessage a b) = SendMessage (symmetricOf a) (symmetricOf b)
    symmetricOf (Spawn a b c d e)
        = Spawn
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

robotsOnBoard :: Board -> [(Int, Int, Robot)]
robotsOnBoard b = concat $ map robotExtractor $ zip coordinates $ map (robotAt b) $ coordinates
    where
    coordinates :: [(Int, Int)]
    coordinates = liftM2 (,) [0..boardSize b - 1] [0..boardSize b - 1]
    robotExtractor :: ((Int, Int), Maybe Robot) -> [(Int, Int, Robot)]
    robotExtractor (_, Nothing) = []
    robotExtractor ((x, y), Just rob) = [(x, y, rob)]

propOrderIndependence :: (Eq b) => ([a] -> [b]) -> [a] -> Int -> Bool
propOrderIndependence f xs seed = sameElements originalOut shuffleOut
    where
    originalOut = f xs
    shuffleOut = f $ shuffle seed xs

propConflictsResolved :: [RobotAndAction] -> (Bool, Bool)
propConflictsResolved acts
    = (allDifferent (map getLocation acts) ,allDifferent finalLocs)
    where
    finalLocs :: [(Int, Int)]
    finalLocs = concat $ map (map loc . finalLocations) $ removeConflicting acts
        where
        loc :: (Int, Int, Bool) -> (Int, Int)
        loc (x, y, _) = (x, y)
    getLocation :: RobotAndAction -> (Int, Int)
    getLocation ((x, y, _), _) = (x, y)

{- Makes the given set of robots symmetric about an axis by adding extra robots.
    The resulting board is guaranteed to be symmetric and have no conflicts. -}
makeSymmetric :: [RobotAndAction] -> [RobotAndAction]
makeSymmetric raas = unique ++ map symmetricOf unique
    where
    unique = filter (\((x,y,_),_) -> x /= y) $ nubBy ((==) `on` (effectivePosition)) raas

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
