module Infsabot.MoveConflictResolution (
    FinalLocations, Remove(..), RobotAndAction,
    removeConflicting, removeConflicting2, finalLocations2, finalLocations, conflictsBetween,
    propOrganizeRobotsSame, RAAFL, FinalLocs(FinalLocs), organizeRobots, firstConflicts
) where

import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Base

import Data.List(groupBy, sortBy)
import Data.Function(on)

import Infsabot.Tools(sameElements, (!-!))

import Infsabot.Debug

type RobotAndAction = ((Int, Int, Robot), RobotAction)

type FinalLocations = [(Int, Int, Bool)]

data Remove = Remove Bool Bool deriving (Show)

type RAAWRemove  = (RobotAndAction, Bool)

--	Removes any moves that would result in two robots being in the same spot.
--	Whichever move is a movement will be removed. If both are movements, both
 	-- are removed

removeConflicting2 :: [RobotAndAction] -> [RobotAndAction]
removeConflicting2 raas
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

{-
    New logic for Move Conflict Resolution.
    Sort Robots by and group by column. Sort columns by y-coordinate.
    Zip together the final locations of each robot with it.
    Go 3 columns at a time:
        Resolve all conflicts robots of the three columns may have with the first column.
            Replace any conflicting moves with Noop.
            If any rightward moves of the left column were deleted
                Flip a flag that will force the recalculation of the previous columns
-}


data FinalLocs = FinalLocs (Maybe (Int, Int)) (Maybe (Int, Int)) deriving (Eq, Show)

type RAAFL = (RobotAndAction, FinalLocs)

type RAA3Col = ([RAAFL], [RAAFL], [RAAFL])

removeConflicting :: [RobotAndAction] -> [RobotAndAction]
removeConflicting = map fst . concat . completeColumnSweeper . organizeRobots


completeColumnSweeper :: [[RAAFL]] -> [[RAAFL]]
completeColumnSweeper d
        | redoH         = completeColumnSweeper new
        | otherwise     = new
    where
    (new, redoH) = columnSweeper d

{- Returns the swept rest of the results, along with a boolean flag indicating
    whether a recalculation would be necessary.-}
columnSweeper :: [[RAAFL]] -> ([[RAAFL]], Bool)
columnSweeper [] = ([], False)
columnSweeper cols_
        | trace ("a', b', c' = " ++ show (a', b', c')) False = undefined
        | resweepNext   = let (u, v) = columnSweeper (a':bccols') in (u, v || resweepThis)
        | otherwise = (a':bccols', resweepThis)
    where
    cols = drop 3 cols_
    (a, b, c) = (cols_ !-! 0, cols_ !-! 1, cols_ !-! 2)
    ((a', b', c'), resweepThis) = removeLocalCompletely (a, b, c)
    (bccols', resweepNext) = columnSweeper (filter (not . null) $ b':c':cols)


{- First element: whether a downward move was nullified, requiring a single element redo.
   Second element: whether a rightward move was nullified, requiring a column redo-}
data Effect = Effect Bool Bool deriving (Show)

combine :: Effect -> Effect -> Effect
combine (Effect a b) (Effect c d) = Effect (a || c) (b || d)

removeLocalCompletely :: RAA3Col -> (RAA3Col, Bool)
removeLocalCompletely d
        | redoV     = let (u, v) = removeLocalCompletely new in (u, v || redoH)
        | otherwise = (new, redoH)
    where
    (new, Effect redoV redoH) = removeLocal d

{-
    Removes conflicts with the given Robot and Action in the three columns recursively.
    Returns
        (Past columns,
            current and future columns (modified),
            whether or not there is a need to recalculate leftwards)
-}
removeLocal :: RAA3Col -> (RAA3Col, Effect)
removeLocal ([], c, r)
        = (([], c, r), Effect False False)
removeLocal (current:l, c, r)
        | trace (
            "Current = " ++ printRobotAndAction (fst current) ++
            "\n\tLCR\n\t" ++ displayRAAL l
            ++ "\n\t" ++ displayRAAL c
            ++ "\n\t" ++ displayRAAL r ++
            "\n\tPrev\n\t" ++ displayRAAL lprev
            ++ "\n\t" ++ displayRAAL cprev
            ++ "\n\t" ++ displayRAAL rprev ++
            "\n\tNext\n\t" ++ displayRAAL lnext
            ++ "\n\t" ++ displayRAAL cnext
            ++ "\n\t" ++ displayRAAL rnext ++
            "\n\tRemove Curr " ++ show removeCurrent ++
            "\n\tEffect this " ++ show effect''
            ) False = undefined
        | otherwise =  ((l'', c'', r''), effect'')
    where
    -- remove any local conflicts
    ((lprev, cprev, rprev),
        (lnext, cnext, rnext),
        removeCurrent,
        effect)
            = removeLcl current (l, c, r)
    -- Remove any future conflicts
    ((mLnext, mCnext, mRnext),
        Effect redoNextV redoNextH)
            = removeLocal (lnext, cnext, rnext)
    l' = lprev ++ [noopifyIf current removeCurrent] ++ mLnext
    c' = cprev ++ mCnext
    r' = rprev ++ mRnext
    effect' = combine effect (Effect False redoNextH)
    (l'', c'', r'', effect'')
        | redoNextV
            = let ((newL', newC', newR'), secondEffect) = removeLocal(l', c', r')
                in (newL', newC', newR', combine effect' secondEffect)
        | otherwise
            = (l', c', r', effect')
{-
    Removes conflicts with the given Robot and Action in the three columns.
    Returns
        (Past columns,
            current and future columns (modified),
            whether the current robot should be noopified,
            the effect of the potential noopifications of the current robot)
-}
removeLcl :: RAAFL -> RAA3Col -> (RAA3Col, RAA3Col, Bool, Effect)
removeLcl current (l, c, r)
        = ((lprev, cprev, rprev),
            (luse' ++ lrest, cuse' ++ crest, ruse' ++ rrest),
            confThis, effect)
    where
    (lprev, lneigh, lrest) = getNeighborhood current l
    (cprev, cneigh, crest) = getNeighborhood current c
    (rprev, rneigh, rrest) = getNeighborhood current r
    confL = map (conflictsBetween current) lneigh
    confC = map (conflictsBetween current) cneigh
    confR = map (conflictsBetween current) rneigh
    luse' = zipWith noopifyIf lneigh (map snd confL)
    cuse' = zipWith noopifyIf cneigh (map snd confC)
    ruse' = zipWith noopifyIf rneigh (map snd confR)
    confThis = any (any fst) [confL, confC, confR]
    confOther = any (any snd) [confL, confC, confR]
    effect
        | confThis || confOther  = effectOf . fst $ current
        | otherwise = Effect False False

effectOf :: RobotAndAction -> Effect
effectOf ((_, _, rob), MoveIn dir)
    | offset == (Offset 0, Offset 1)   = Effect True True
    | offset == (Offset 1, Offset 0)   = Effect True True
        where offset = getOffset (robotTeam rob) dir
effectOf _ = Effect True True

noopifyIf :: RAAFL -> Bool -> RAAFL
noopifyIf x True = noopify x
noopifyIf x False = x

firstConflicts :: FinalLocs -> FinalLocs -> Bool
firstConflicts (FinalLocs _ xy@(Just _)) (FinalLocs a b)
    = xy == a || xy == b
firstConflicts _ _ = False

conflictsBetween :: RAAFL -> RAAFL -> (Bool, Bool)
conflictsBetween u v = (on firstConflicts snd u v, on firstConflicts snd v u)
{-
Converts the given action into a Noop
-}
noopify :: RAAFL -> RAAFL
noopify (((x, y, rob), _), _) = (newRobAct, finalLocations2 newRobAct)
    where newRobAct = ((x, y, rob), Noop)
{- Splits the given list into three parts
    - the part before the neighborhood of the given robot
    - the neighborhood of the given robot
    - the part after the neighborhood of the given robot
    Where neighborhood is defined as being able to conflict with the given robot
    This code assumes that the robots it is given are an ordered column
-}
getNeighborhood :: RAAFL -> [RAAFL] -> ([RAAFL], [RAAFL], [RAAFL])
getNeighborhood cur rest = (before, filter (/=cur) during, after)
    where
    (before, duringafter) = span (\x -> getRelPosition cur x == Before) rest
    (during, after) = span (\x -> getRelPosition cur x == Neighborhood) duringafter

data RelPosition = Before | Neighborhood | After deriving (Eq, Show)

getRelPosition :: RAAFL -> RAAFL -> RelPosition
getRelPosition a@(((_, y1, _), _), _) b@(((_, y2, _),_), _)
    | inNeighborhood a b        = Neighborhood
    | y1 > y2                   = Before
    | otherwise                 = After

{- Returns whether or not the first robot is in the neighborhood of the second
    Since the maximum effect of a move is 1 spot, the robots must be within
    a taxicab distance of 2-}
inNeighborhood :: RAAFL -> RAAFL -> Bool
inNeighborhood (((x1, y1, _), _), _) (((x2, y2, _),_), _)
    = abs (x1 - x2) + abs (y1 - y2) <= 2

propOrganizeRobotsSame :: [RobotAndAction] -> Bool
propOrganizeRobotsSame us = sameElements us . map fst . concat . organizeRobots $ us

{- Converts an unordered list of RAAs into columns of
    y-sorted robots grouped and sorted by x-coordinate,
    along with the robot's final position.
    -}
organizeRobots :: [RobotAndAction] -> [[RAAFL]]
organizeRobots =
        map (map $ \x -> (x, finalLocations2 x)) .
        map (sortBy (compare `on` yc)) .
        groupBy ((==) `on` xc) .
        sortBy (compare `on` xc)
    where
    xc ((x, _, _), _) = x
    yc ((_, y, _), _) = y

finalLocations2 :: RobotAndAction -> FinalLocs
finalLocations2 ((x,y,rob), act) = locs act
    where
    locs (MoveIn dir)
        = let (newx, newy) = applyOffset (getOffset (robotTeam rob) dir) (x,y)
            in FinalLocs Nothing (Just (newx, newy))
    locs spawn@(Spawn _ _ _ _ _)
        = let (newx, newy) = applyOffset (getOffset (robotTeam rob) $ newDirection spawn) (x,y)
            in FinalLocs (Just (x, y)) (Just (newx, newy))
    locs Die = FinalLocs Nothing Nothing
    locs _ = FinalLocs (Just (x, y)) Nothing
