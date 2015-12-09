{-# Language TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Infsabot.MoveConflictResolution.Tests(
    mcrChecks, individualMCRChecks) where

import Infsabot.MoveConflictResolution.Logic

import Infsabot.Tools.Interface

import Data.List(nubBy)
import Data.Function(on)
import Data.Tuple(swap)
import Data.Monoid(mconcat)

import Test.QuickCheck hiding (shuffle)
import Data.DeriveTH(derive, makeArbitrary)

import Infsabot.RobotAction.Tests()
import Infsabot.Test.TestLibrary

individualMCRChecks :: [RobotAndAction] -> TestResult String
individualMCRChecks raas
    = mconcat $
        zipWith constructTest
        (map ($ raas)
            [propNoChangeInLength, propSymmeteryPreserving, uncurry (~~>) . propConflictsResolved, propOrganizeRobotsSame])
        ["Conflict Order Independence", "No Change In Length", "Symmetery Preserving", "Conflicts Resolved", "Organize Robots Same"]

mcrChecks :: [IO Result]
mcrChecks = [
    putStrLn "Conflict Order Independence" >> doChecks 5 propConflictOrderIndependence,
    putStrLn "No Change In Length" >> doChecks 1 propNoChangeInLength,
    putStrLn "Symmetery Preserving" >> doChecks 5 propSymmeteryPreserving,
    putStrLn "Conflicts Resolved" >> doChecks 50 (uncurry (==>) . propConflictsResolved),
    putStrLn "Organize Robots Same" >> doChecks 1 propOrganizeRobotsSame]

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
    unique = removeOpposition $ nubBy ((==) `on` effectivePosition) raas
    removeOpposition
        = foldr
            remover
            []
    remover :: RobotAndAction -> [RobotAndAction] -> [RobotAndAction]
    remover a = filter (\ u -> effectivePosition a /= swap (effectivePosition u))

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
    finalLocs = concatMap (finalLocsToList . finalLocations) $ removeConflicting acts

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

$( derive makeArbitrary ''FinalLocs )
