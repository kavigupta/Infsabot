module Infsabot.MoveConflictResolution (
    FinalLocations, Remove(..), RobotAndAction,
    doConflict, removeConflicting, finalLocations, locationConflicts, removeAll
) where

import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Base

--import Debug.Trace

type RobotAndAction = ((Int, Int, Robot), RobotAction)

type FinalLocations = [(Int, Int, Bool)]

data Remove = Remove Bool Bool deriving (Show)

type RAAWRemove  = (RobotAndAction, Bool)

--	Removes any moves that would result in two robots being in the same spot.
--	Whichever move is a movement will be removed. If both are movements, both
 	-- are removed

removeConflicting :: [RobotAndAction] -> [RobotAndAction]
removeConflicting raas
        | raas == raas'   = raas
        | otherwise     = removeConflicting raas'
    where
    raas' :: [RobotAndAction]
    raas' = map downgrade $ removeAll $ map (\x -> (x, False)) raas
        where
        downgrade :: RAAWRemove -> RobotAndAction
        downgrade ((xyrob, _), True) = (xyrob, Noop)
        downgrade (raa, False) = raa

removeAll :: [RAAWRemove] -> [RAAWRemove]
removeAll [] = []
removeAll (x:xs) = current : removeAll rest
    where
    (current, rest) = getRemoveStatus x xs

getRemoveStatus :: RAAWRemove -> [RAAWRemove] -> (RAAWRemove, [RAAWRemove])
getRemoveStatus (raa, initialRemove) raas
        = ((raa, initialRemove || removeThis), others)
    where
    others = map (\((x, a), b) -> (x, a || b)) $ zip raas shouldRemove
    shouldRemove = map (\(Remove _ other) -> other) $ restConflicts
    removeThis = any (\(Remove this _) -> this) $ restConflicts
    restConflicts :: [Remove]
    restConflicts = map (conflictsWithFirst . fst) raas
        where
        conflictsWithFirst other = doConflict (finalLocations raa) (finalLocations other)

merge :: Remove -> Remove -> Remove
merge (Remove a b) (Remove c d) = Remove (a || c) (b || d)

doConflict :: FinalLocations -> FinalLocations -> Remove
doConflict [] _ = Remove False False
doConflict (a:rest) other = merge (locationConflicts a other) (doConflict rest other)

locationConflicts :: (Int, Int, Bool) -> FinalLocations -> Remove
locationConflicts _ [] = Remove False False
locationConflicts (x1, y1, keep1) ((x2, y2, keep2):rest)
    | (x1, y1) == (x2, y2)
        = merge (Remove (not keep1) (not keep2)) conflictsInRest
    | otherwise     = conflictsInRest
    where
    conflictsInRest = locationConflicts (x1, y1, keep1) rest

finalLocations :: RobotAndAction -> FinalLocations
finalLocations ((x,y,rob), act) = locs act
    where
    locs (MoveIn dir)
        = let (newx, newy) = applyOffset (getOffset (robotTeam rob) dir) (x,y)
            in [(newx, newy, False)]
    locs spawn@(Spawn _ _ _ _ _)
        = let (newx, newy) = applyOffset (getOffset (robotTeam rob) $ newDirection spawn) (x,y)
            in [(newx, newy, False), (x, y, True)]
    locs Die = []
    locs _ = [(x,y, True)]
