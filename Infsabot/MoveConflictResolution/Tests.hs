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

import Test.QuickCheck hiding (shuffle)
import Data.DeriveTH(derive, makeArbitrary)

import Infsabot.RobotAction.Tests()
import Infsabot.Test.TestLibrary

individualMCRChecks :: Maybe Int -> [RobotAndAction] -> TestResult String
individualMCRChecks size raas
    = mconcat $
        zipWith constructTest
        (map ($ raas)
            [propNoChangeInLength size, propSymmeteryPreserving size, uncurry (~~>) . propConflictsResolved size, propOrganizeRobotsSame])
        ["Conflict Order Independence", "No Change In Length", "Symmetery Preserving", "Conflicts Resolved", "Organize Robots Same"]

mcrChecks :: [IO Result]
mcrChecks = [
    putStrLn "Conflict Order Independence" >> doChecks 5 propConflictOrderIndependence,
    putStrLn "No Change In Length" >> doChecks 1 propNoChangeInLength,
    putStrLn "Symmetery Preserving" >> doChecks 5 propSymmeteryPreserving,
    putStrLn "Conflicts Resolved" >> doChecks 50 (uncurry (==>) . propConflictsResolved (Just 100)),
    putStrLn "Conflicts Resolved" >> doChecks 50 (uncurry (==>) . propConflictsResolved Nothing),
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

propConflictsResolved :: Maybe Int -> [RobotAndAction] -> (Bool, Bool)
propConflictsResolved size acts
    = (allDifferent (map (positionOf . fst) acts), allDifferent finalLocs)
    where
    finalLocs :: [(Int, Int)]
    finalLocs = concatMap (finalLocsToList . finalLocations) $ removeConflicting size acts

propOrganizeRobotsSame :: [RobotAndAction] -> Bool
propOrganizeRobotsSame us = sameElements us . map fst . concat . organizeRobots $ us

propSymmeteryPreserving :: Maybe Int -> [RobotAndAction] -> Bool
propSymmeteryPreserving size raas
    = teamSymmetric
            (removeConflicting size $ makeSymmetric raas) == TRSuccess

propConflictOrderIndependence :: (RAAFL, RAAFL) -> Property
propConflictOrderIndependence (x, y) = positionOf x /= positionOf y
        ==> a == c && b == d
    where
    (a, b) = conflictsBetween x y
    (d, c) = conflictsBetween y x

propNoChangeInLength :: Maybe Int -> [RobotAndAction] -> Bool
propNoChangeInLength size r = length raas == length (removeConflicting size raas)
    where raas = nubBy ((==) `on` positionOf) r

$( derive makeArbitrary ''FinalLocs )
