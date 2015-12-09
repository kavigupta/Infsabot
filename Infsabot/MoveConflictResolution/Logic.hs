module Infsabot.MoveConflictResolution.Logic(
        removeConflicting,
        RobotAndAction, RAAFL,
        FinalLocs(..),
        finalLocations,
        organizeRobots,
        conflictsBetween
    ) where

import Infsabot.Robot.Interface

import Infsabot.RobotAction.Interface
import Infsabot.Base.Interface

import Data.List(groupBy, sortBy)
import Data.Function(on)

import Infsabot.Tools.Interface(spanNeq, (!-!))

import Infsabot.Debug

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

{-
    Removes all conflicting Robot Actions by converting them to Noop.
    This is guaranteed to be fair to both teams, cancelling the moves of both
    robots if there is a conflict.

    Robots are protected at their original positions: any move
        onto these positions is removed.
    If two robots try to move or spawn into the same position,
        both are cancelled.
-}
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
    rest = drop 3 cols_
    (a, b, c) = (cols_ !-! 0, cols_ !-! 1, cols_ !-! 2)
    ((a', b', c'), resweepThis) = removeLocalCompletely (a, b, c)
    (bccols', resweepNext) = columnSweeper (filter (not . null) $ b':c':rest)



removeLocalCompletely :: RAA3Col -> (RAA3Col, Bool)
removeLocalCompletely d
        | redo     = let (u, v) = removeLocalCompletely new in (u, v || redo)
        | otherwise = (new, redo)
    where
    (new, redo) = removeLocal d

{-
    Removes conflicts with the given Robot and Action in the three columns recursively.
    Returns
        (Past columns,
            current and future columns (modified),
            whether or not there is a need to recalculate leftwards)
-}
removeLocal :: RAA3Col -> (RAA3Col, Bool)
removeLocal ([], c, r)
        = (([], c, r), False)
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
        redoNext)
            = removeLocal (lnext, cnext, rnext)
    l' = lprev ++ [noopifyIf current removeCurrent] ++ mLnext
    c' = cprev ++ mCnext
    r' = rprev ++ mRnext
    effect' = effect || redoNext
    (l'', c'', r'', effect'')
        | redoNext
            = let ((newL', newC', newR'), secondEffect) = removeLocal(l', c', r')
                in (newL', newC', newR', effect' || secondEffect)
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
removeLcl :: RAAFL -> RAA3Col -> (RAA3Col, RAA3Col, Bool, Bool)
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
    confOtherL = map snd confL
    confOtherC = map snd confC
    confOtherR = map snd confR
    luse' = zipWith noopifyIf lneigh confOtherL
    cuse' = zipWith noopifyIf cneigh confOtherC
    ruse' = zipWith noopifyIf rneigh confOtherR
    effOther = any or
        [zipWith effectOf confOtherL (map fst lneigh),
            zipWith effectOf confOtherC (map fst cneigh),
            zipWith effectOf confOtherR (map fst rneigh)]
    confThis = any (any fst) [confL, confC, confR]
    effect = (effectOf confThis . fst $ current) || effOther
    --effect = effectOf confThis (fst current) `mappend` (if not confOther then Effect False False else Effect True True)

effectOf :: Bool -> RobotAndAction -> Bool
effectOf True (_, MoveIn _) = True
    -- | x /= 0    = Effect False True
    -- | y /= 0    = Effect True False
        --where (Offset x, Offset y) = getOffset (robotTeam rob) dir
effectOf _ _ = False

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
noopify ((xyrob, _), _) = (newRobAct, finalLocations newRobAct)
    where newRobAct = (xyrob, Noop)
{- Splits the given list into three parts
    - the part before the neighborhood of the given robot
    - the neighborhood of the given robot
    - the part after the neighborhood of the given robot
    Where neighborhood is defined as being able to conflict with the given robot
    This code assumes that the robots it is given are an ordered column
-}
getNeighborhood :: RAAFL -> [RAAFL] -> ([RAAFL], [RAAFL], [RAAFL])
getNeighborhood = liftPosition go
    where
    go (x1, y1) rest = (before, during, after)
        where
        isBefore (x2, y2) = (y1 > y2) && not (inNeighborhood (x1, y1) (x2, y2))
        isDuring (x2, y2) = inNeighborhood (x1, y1) (x2, y2)
        notSame (x2, y2) = (x1, y1) /= (x2, y2)
        (before, duringafter) = span (liftPosition isBefore) rest
        (during, after) = spanNeq (liftPosition isDuring) (liftPosition notSame) duringafter

{- Returns whether or not the first robot is in the neighborhood of the second
    Since the maximum effect of a move is 1 spot, the robots must be within
    a taxicab distance of 2-}
inNeighborhood :: (Int, Int) -> (Int, Int) -> Bool
inNeighborhood (x1, y1) (x2, y2)
    = abs (x1 - x2) + abs (y1 - y2) <= 2

{- Converts an unordered list of RAAs into columns of
    y-sorted robots grouped and sorted by x-coordinate,
    along with the robot's final position.
    -}
organizeRobots :: [RobotAndAction] -> [[RAAFL]]
organizeRobots =
        map (map (\x -> (x, finalLocations x)) . sortBy (compare `on` yc)) .
        groupBy ((==) `on` xc) .
        sortBy (compare `on` xc)
    where
    xc = fst . getLocation . fst
    yc = snd . getLocation . fst

finalLocations :: RobotAndAction -> FinalLocs
finalLocations (PositionedRobot ((x,y),rob), act) = locs act
    where
    locs (MoveIn dir)
        = let (newx, newy) = applyDirection (robotTeam rob) dir (x,y)
            in FinalLocs Nothing (Just (newx, newy))
    locs (Spawn spawn)
        = let (newx, newy) = applyDirection (robotTeam rob) (newDirection spawn) (x,y)
            in FinalLocs (Just (x, y)) (Just (newx, newy))
    locs Die = FinalLocs Nothing Nothing
    locs _ = FinalLocs (Just (x, y)) Nothing

liftPosition :: ((Int, Int) -> a) -> RAAFL -> a
liftPosition f ((PositionedRobot ((x2, y2), _),_), _) = f (x2, y2)
