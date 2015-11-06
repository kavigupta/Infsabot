module Infsabot.MoveConflictResolution (RobotAndAction, removeConflicting) where

import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Base

type RobotAndAction = ((Int, Int, Robot), RobotAction)

type FinalLocations = [(Int, Int, Bool)]

data Remove = Remove Bool Bool

--	Removes any moves that would result in two robots being in the same spot.
--	Whichever move is a movement will be removed. If both are movements, both
 	-- are removed
removeConflicting :: [RobotAndAction] -> [RobotAndAction]
removeConflicting [] = []
removeConflicting (raa:raas)
    | removeThis    = removeConflicting restNoConflicts
    | otherwise     = raa:removeConflicting restNoConflicts
    where
    restNoConflicts
        = map fst
            $ filter (\(_, Remove _ removeOther) -> not removeOther)
            $ zip raas restConflicts
    removeThis = any (\(Remove this _) -> this) restConflicts
    restConflicts =  map conflictsWithThis raas
        where
        conflictsWithThis other = isConflict flThis (finalLocations other)
            where flThis = finalLocations raa

merge :: Remove -> Remove -> Remove
merge (Remove a b) (Remove c d) = Remove (a || b) (c || d)

isConflict :: FinalLocations -> FinalLocations -> Remove
isConflict [] _ = Remove False False
isConflict (a:rest) other = merge (locationConflicts a other) (isConflict rest other)

locationConflicts :: (Int, Int, Bool) -> FinalLocations -> Remove
locationConflicts _ [] = Remove False False
locationConflicts (x1, y1, keep1) ((x2, y2, keep2):rest)
    | (x1, y2) == (x2, y2)
        = case (keep1, keep2) of
            (True, True)    -> merge (Remove False True) $ conflictsInRest
            (True, False)   -> Remove False True
            (False, True)   -> conflictsInRest
            _               -> Remove True True
    | otherwise     = Remove False False
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
