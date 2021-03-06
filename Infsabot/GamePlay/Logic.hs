{-# Language TupleSections #-}
{-# Language DeriveFunctor #-}
module Infsabot.GamePlay.Logic(
        boards, boardsAndActions, Game(..), isVictory, limit
    ) where

import Infsabot.Board.Interface
import Infsabot.Robot.Interface

import Infsabot.RobotAction.Interface
import Infsabot.Parameters
import Infsabot.MoveConflictResolution.Interface
import Infsabot.Base.Interface
import Data.List(sortBy)
import Data.Function(on)

import Data.Maybe(fromJust)
import Infsabot.Tools.Interface

import Infsabot.Debug

type RobotAndResult = (PositionedRobot, RobotProgramResult)

data Game a = Victory Team | a :~ Game a
    deriving Functor

limit :: Int -> Game a -> (Maybe Team, [a])
limit _ (Victory t) = (Just t, [])
limit 0 _ = (Nothing, [])
limit n (x:~xs) = let (u, v) = limit (n-1) xs in (u, x:v)

boards :: Parameters -> Board -> Game Board
boards params = fmap fst . boardsAndActions params

boardsAndActions :: Parameters -> Board -> Game (Board, [((Int, Int), RobotAction)])
boardsAndActions params b = case isVictory b of
        Nothing -> (b, a) :~ boardsAndActions params b'
        Just team -> Victory team
    where
    (b', a) = play params b

-- the main play function. This executes all robot actions and updates the board.
play :: Parameters -> Board -> (Board, [((Int, Int), RobotAction)])
play p b
    | trace ("actions = " ++ show actions ++ "\n\t"
        ++ show (map (possibleAction p) actions) ++ "\n\t"
        ++ show (map printRobotAndAction $ removeConflicting (Just $ boardSize b) $ map (possibleAction p) actions)) False = undefined
     | otherwise =
        (newBoard, map extractXYrob actions)
    where
        newBoard =
            -- apply actions
            actionApplier .
            -- apply all action costs
            actionCostApplier .
            -- apply the time tick
            applyTimeTick $
            b
        -- actions := results - state.
        -- hardDriveUpdater updates the hard drive
        extractXYrob (PositionedRobot (xy, r), u) = (xy, deRelativize (robotTeam r) u)
        actions
            = updateAllHardDrives $ getRobotResults p b
        -- resolves and sorts the actions
        resolvedAndSortedActions
            = sortBy (compare `on` (orderOfOperations . snd))
                $ removeConflicting (Just $ boardSize b)
                $ map (possibleAction p) actions
        -- gets the function which applies the costs of the actions
        actionCostApplier = applyActionCosts p resolvedAndSortedActions
        -- gets the function which applies all actions
        actionApplier
            = foldl (.) id $ map (getAction p) resolvedAndSortedActions

isVictory :: Board -> Maybe Team
isVictory b = case foldr combine (False, False) $ listOfRobots b of
        (True, False) -> Just A
        (False, True) -> Just B
        _             -> Nothing
    where
    combine :: PositionedRobot -> (Bool, Bool) -> (Bool, Bool)
    combine (PositionedRobot (_, r)) (e, f) = (e || t == A, f || t == B)
        where t = robotTeam r

deRelativize :: Team -> RobotAction -> RobotAction
deRelativize team act = case act of
        (MoveIn d) -> MoveIn $ derel d
        (Fire f) -> Fire f {fireDirection=derel $ fireDirection f}
        (Send s) -> Send s {sendDirection=derel $ sendDirection s}
        (Spawn s) -> Spawn s {newDirection=derel $ newDirection s, newProgram= derelInFirst . newProgram s}
        x -> x
    where
    derelInFirst (x, y) = (deRelativize team x, y)
    derel x = oppositeDirection $ case team of
        B -> x
        A -> case x of
            N -> W
            W -> N
            E -> S
            S -> E

-- gets the action associated with the given robotandaction in the form
-- of a function that mutates a board
getAction :: Parameters -> RobotAndAction -> Board -> Board
getAction _ (PositionedRobot ((x,y),_),Die) b
                                    = setRobot (x, y) Nothing b
getAction _ (_, Noop) b             = b
getAction p (PositionedRobot ((x, y), rob), Send send) b
                                    = mutateRobot
                                        (robotTeam rob)
                                        (x, y)
                                        (sendDirection send)
                                        (unNatural $ paramLineOfMessageSending p)
                                        sendAction
                                        b
    where
    sendAction toReceive
            = Just $ toReceive { robotMessages = newMessage: robotMessages toReceive }
        where
        newMessage :: (String, RDirection)
        newMessage = (messageToSend send, oppositeDirection $ sendDirection send)
getAction p (PositionedRobot ((x, y), rob), Fire fire) b
                                    = mutateRobot
                                        (robotTeam rob)
                                        (x, y)
                                        (fireDirection fire)
                                        (unNatural $ paramLineOfFire p)
                                        fireAction
                                        b
    where
    fireAction toReceive
            | newHP > 0     = Just $ toReceive { robotHitpoints = newHP }
            | otherwise     = Nothing
        where newHP = robotHitpoints toReceive - unNatural (apply (paramHPRemoved p) (materialExpended fire))
getAction _ (PositionedRobot ((x, y), _), Dig) b
        | mat == SpotMaterial       = updateSpot (x,y) SpotEmpty b
        | otherwise                 = b
        where GameSpot mat _ = fromJust $ b !!! (x, y)
getAction _ (PositionedRobot ((x, y), rob), MoveIn dir) b
                                    = setRobot (x, y) Nothing $ setRobot (newx, newy) (Just rob) b
    where (newx, newy) = applyDirection (robotTeam rob) dir (x, y)
getAction params (PositionedRobot ((x, y), rob), Spawn spawn) b
                                    = setRobot (newx, newy) (Just newRobot) b
    where
    newRobot = Robot {
        robotProgram = newProgram spawn,
        robotTeam = robotTeam rob,
        robotAppearance = newAppearance spawn,
        robotMaterial = unNatural $ paramInitialMaterial params,
        robotHitpoints = unNatural $ paramInitialHP params,
        robotBirthdate = boardTime b,
        robotMemory = newMemory spawn,
        robotMessages = []
    }
    (newx, newy) = applyDirection (robotTeam rob) (newDirection spawn) (x, y)

-- Gets a function that mutates a robot along a path, given
    -- an original position
    -- a direction to move in
    -- a maximum number of paces
    -- a function that takes a robot and returns
        -- Just (the updated robot)
        -- Nothing (the robot will be deleted)
mutateRobot :: Team -> (Int, Int) -> RDirection -> Int -> (Robot -> Maybe Robot) -> Board -> Board
mutateRobot team (x, y) direction distance mutator b
        = case maybeRobot of
            Just (_, _, toReceive)  -> setRobot (x, y) (mutator toReceive) b
            Nothing                 -> b
    where
    maybeRobot = robotAlongPath team b (x, y) direction distance

-- Takes a list of robots and results and outputs a list of robots with updated
    -- hard drives and their actions
updateAllHardDrives :: [RobotAndResult] -> [RobotAndAction]
updateAllHardDrives = map removeState
    where
    removeState :: RobotAndResult -> RobotAndAction
    removeState (PositionedRobot (xy, rob), (act, state)) = (PositionedRobot (xy, rob {robotMemory = state}), act)

-- Takes a parameter list and list of actions, and applies all their costs to
-- the given board.
applyActionCosts :: Parameters -> [RobotAndAction] -> Board -> Board
applyActionCosts params = foldr ((.) . applyActionCost) id
    where
    applyActionCost :: RobotAndAction -> Board -> Board
    applyActionCost (PositionedRobot ((x, y), rob), act) = setRobot (x, y) $ Just newRobot
        where
        cost = actionCost params act
        newRobot = rob {robotMaterial = robotMaterial rob - unNatural cost}

-- Gets a list of robots and their program results
getRobotResults :: Parameters -> Board -> [RobotAndResult]
getRobotResults p b = map getRobotResult $ listOfRobots b
    where
    getRobotResult prob@(PositionedRobot (_, rob)) = (prob, robotProgram rob state)
       where state = getKnownState p (robotTeam rob) b prob

-- Gets the known state for the given robot
getKnownState :: Parameters -> Team -> Board -> PositionedRobot -> KnownState
getKnownState p team b (PositionedRobot ((x, y), rob)) = KnownState {
        peekAtSpot = peekFn,
        material = robotMaterial rob,
        stateLocation = (x,y),
        stateAge = boardTime b - robotBirthdate rob,
        stateMemory = robotMemory rob,
        stateMessages = robotMessages rob
    }
    where
    peekFn :: [RDirection] -> Maybe SeenSpot
    peekFn directs = toSeenSpot <$> (limitedOffset team (unNatural $ paramLineOfSight p) directs (x, y)
            >>= (b !!!))

-- Returns the closest approximation to the requested action that is possible
    -- given the robot's level of material
-- This may be another type of action.
possibleAction :: Parameters -> RobotAndAction -> RobotAndAction
possibleAction p (xyrob@(PositionedRobot (_, rob)), action)
    | actionCost p action <= makeNatural (robotMaterial rob)  = (xyrob, action)
    | otherwise                                               = downgrade action
    where
    downgrade :: RobotAction -> RobotAndAction
    downgrade Die = (xyrob, Die)
    downgrade Noop = (xyrob, Die) -- no alternative
    downgrade (MoveIn _) = tryNoop
    downgrade Dig = tryNoop
    downgrade (Spawn s) -- TODO Potential massive inefficiency here!
        | newMaterial s == 0
            = tryNoop
        | otherwise
            = possibleAction p (xyrob, Spawn $ s {newMaterial = makeNatural $ unNatural (newMaterial s) - 1})
    downgrade (Fire f)
        | materialExpended f == 0
            = tryNoop
        |otherwise
            = possibleAction p (xyrob, Fire $ f {materialExpended = makeNatural $ unNatural (materialExpended f) - 1})
    downgrade (Send _) = tryNoop
    tryNoop = possibleAction p (xyrob, Noop)

-- Increments time by one
applyTimeTick :: Board -> Board
applyTimeTick b = b {boardTime = boardTime b + 1}
