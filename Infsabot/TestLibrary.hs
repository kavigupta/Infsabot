module Infsabot.TestLibrary(robotsOnBoard, propOrderIndependence) where

import Infsabot.Board
import Infsabot.Robot
import Control.Monad
import Infsabot.Tools

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
