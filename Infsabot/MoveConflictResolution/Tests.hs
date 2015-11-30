{-# Language TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Infsabot.MoveConflictResolution.Tests(
    propConflictsResolved,
    propOrganizeRobotsSame,
    propSymmeteryPreserving,
    propConflictOrderIndependence,
    propNoChangeInLength) where

import Infsabot.MoveConflictResolution.Logic

import Infsabot.Tools
import Infsabot.TestLibrary
import Data.List(nubBy)
import Data.Function(on)
import Data.Tuple(swap)
import Test.QuickCheck hiding (shuffle)
import Data.DeriveTH(derive, makeArbitrary)

$( derive makeArbitrary ''FinalLocs )

instance TeamedObject RAAFL where
    positionOf = positionOf . fst
    teamOf = teamOf . fst

instance TeamedComparable RAAFL where
    areSymm (x, y) = areSymm (fst x, fst y)
    symmetricOf (raa, _) = (symmetricOf raa, finalLocations $ symmetricOf raa)

{- Makes the given set of robots symmetric about an axis by adding extra robots.
    The resulting board is guaranteed to be symmetric and have no conflicts. -}
makeSymmetric :: [RobotAndAction] -> [RobotAndAction]
makeSymmetric raas = unique ++ map symmetricOf unique
    where
    unique = removeOpposition $ nubBy ((==) `on` (effectivePosition)) raas
    removeOpposition [] = []
    removeOpposition (a:us) = filter (\u -> effectivePosition a /= swap (effectivePosition u)) $ removeOpposition us

finalLocsToList :: FinalLocs -> [(Int, Int)]
finalLocsToList (FinalLocs x y) = tl x ++ tl y
    where
    tl Nothing = []
    tl (Just l) = [l]

propConflictsResolved :: [RobotAndAction] -> (Bool, Bool)
propConflictsResolved acts
    = (allDifferent (map (positionOf . fst) acts), allDifferent finalLocs)
    where
    finalLocs :: [(Int, Int)]
    finalLocs = concat $ map (finalLocsToList . finalLocations) $ removeConflicting acts

propOrganizeRobotsSame :: [RobotAndAction] -> Bool
propOrganizeRobotsSame us = sameElements us . map fst . concat . organizeRobots $ us

propSymmeteryPreserving :: [RobotAndAction] -> Bool
propSymmeteryPreserving raas
    = teamSymmetric
            (removeConflicting $ makeSymmetric raas) == TRSuccess

propConflictOrderIndependence :: (RAAFL, RAAFL) -> Property
propConflictOrderIndependence (x, y) = positionOf x /= positionOf y
        ==> a == c && b == d
    where
    (a, b) = conflictsBetween x y
    (d, c) = conflictsBetween y x


propNoChangeInLength :: [RobotAndAction] -> Bool
propNoChangeInLength r = length raas == length (removeConflicting raas)
    where raas = nubBy ((==) `on` positionOf) r
