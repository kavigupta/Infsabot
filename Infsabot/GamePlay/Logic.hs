{-# Language TupleSections #-}
module Infsabot.GamePlay.Logic(boards) where

import Infsabot.Board.Interface
import Infsabot.Robot.Interface

import Infsabot.RobotAction.Interface
import Infsabot.Parameters
import Infsabot.MoveConflictResolution.Interface
import Infsabot.Base.Interface
import Data.List(sortBy)
import Data.Function(on)
import Control.Monad(liftM)
import Data.Maybe(fromJust)
import Infsabot.Tools.Interface

import Infsabot.Debug

type RobotAndResult = (PositionedRobot, RobotProgramResult)

boards :: Parameters -> Board -> [Board]
boards params = iterate (play params)

-- the main play function. This executes all robot actions and updates the board.
play :: Parameters -> Board -> Board
play p b
	| trace ("actions = " ++ show actions ++ "\n\t"
        ++ show (map (possibleAction p) actions) ++ "\n\t"
        ++ show (map printRobotAndAction $ removeConflicting $ map (possibleAction p) actions)) False = undefined
 	| otherwise =
		-- apply actions
		actionApplier .
		-- apply all action costs
		actionCostApplier .
		-- update all hard drives
		hardDriveUpdater .
		-- apply the time tick
		applyTimeTick $
		b
	where
		-- actions := results - state.
		-- hardDriveUpdater updates the hard drive
		(hardDriveUpdater, actions)
			= updateAllHardDrives $ getRobotResults p b
		-- resolves and sorts the actions
		resolvedAndSortedActions
			= sortBy (compare `on` (orderOfOperations . snd))
				$ removeConflicting
				$ map (possibleAction p) actions
		-- gets the function which applies the costs of the actions
		actionCostApplier = applyActionCosts p resolvedAndSortedActions
		-- gets the function which applies all actions
		actionApplier
			= foldl (.) id $ map (getAction p) resolvedAndSortedActions

-- gets the action associated with the given robotandaction in the form
-- of a function that mutates a board
getAction :: Parameters -> RobotAndAction -> Board -> Board
getAction _ (PositionedRobot ((x,y),_),Die) b
									= setRobot (x, y) Nothing b
getAction _ (_, Noop) b		 	= b
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
			| newHP > 0 	= Just $ toReceive { robotHitpoints = newHP }
			| otherwise		= Nothing
		where newHP = robotHitpoints toReceive - unNatural (apply (paramHPRemoved p) (materialExpended fire))
getAction _ (PositionedRobot ((x, y), _), Dig) b
		| mat == SpotMaterial		= updateSpot (x,y) SpotEmpty b
		| otherwise					= b
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
			Just (_, _, toReceive) 	-> setRobot (x, y) (mutator toReceive) b
			Nothing					-> b
	where
	maybeRobot = robotAlongPath team b (x, y) direction distance

-- Takes a list of robots and results and outputs
	-- (a function that updates a board to one with hard drives updated,
	-- 	a list of robots and their actions)
-- This is performed before any robot actions are carried out.
updateAllHardDrives :: [RobotAndResult] -> (Board -> Board, [RobotAndAction])
updateAllHardDrives rars = (
		foldr ((.) . updateHardDrive) id rars,
		map removeState rars)
	where
	removeState (xyrob, (act, _)) = (xyrob, act)
	updateHardDrive :: RobotAndResult -> Board -> Board
	updateHardDrive (PositionedRobot ((x,y),rob), (_, state))
		= setRobot (x,y) $ Just rob {robotMemory = state}

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
	peekFn directs = liftM toSeenSpot $ limitedOffset team (unNatural $ paramLineOfSight p) directs (x, y)
			>>= (b !!!)

-- Returns the closest approximation to the requested action that is possible
    -- given the robot's level of material
-- This may be another type of action.
possibleAction :: Parameters -> RobotAndAction -> RobotAndAction
possibleAction p (xyrob@(PositionedRobot (_, rob)), action)
    | actionCost p action <= makeNatural (robotMaterial rob)  = (xyrob, action)
    | otherwise 										      = downgrade action
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
