module Infsabot.TestLibrary(robotsOnBoard,
    propOrderIndependence,
    propConflictsResolved) where

import Infsabot.Board
import Infsabot.Robot
import Control.Monad
import Infsabot.Tools
import Infsabot.MoveConflictResolution

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
