{-# Language TupleSections #-}
module Infsabot.GamePlay(boards, RobotAndAction) where

import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Parameters
import Infsabot.Base
import Data.List(sortBy)
import Data.Function(on)

import Debug.Trace
import Infsabot.Debug

type RobotAndResult = ((Int, Int, Robot), RobotProgramResult)
type RobotAndAction = ((Int, Int, Robot), RobotAction)


boards :: Parameters -> Board -> [Board]
boards params initialBoard = iterate (play params) initialBoard

-- the main play function. This executes all robot actions and updates the board.
play :: Parameters -> Board -> Board
play p b
    | trace (show (map printRobotAndAction actions)
        ++ "\n\t"
        ++ show (map printRobotAndAction resolvedAndSortedActions)) False = undefined
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
				$ removeCompetingMoves
				$ map (possibleAction p) actions
		-- gets the function which applies the costs of the actions
		actionCostApplier = applyActionCosts p resolvedAndSortedActions
		-- gets the function which applies all actions
		actionApplier
			= foldl (.) id $ map (getAction p) resolvedAndSortedActions

-- gets the action associated with the given robotandaction in the form
-- of a function that mutates a board
getAction :: Parameters -> RobotAndAction -> Board -> Board
getAction _ ((x,y,_),Die) b 		= setRobot (x, y, Nothing) b
getAction _ (_, Noop) b		 	= b
getAction p ((x, y, rob), send@(SendMessage _ _)) b
									= mutateRobot
										(robotTeam rob)
										(x, y)
										(sendDirection send)
										(lineOfMessageSending p)
										sendAction
										b
	where
	sendAction toReceive
			= Just $ toReceive { robotMessages = newMessage: robotMessages toReceive }
		where
		newMessage :: (String, RDirection)
		newMessage = (messageToSend send, oppositeDirection $ sendDirection send)
getAction p ((x, y, rob), fire@(Fire _ _)) b
									= mutateRobot
										(robotTeam rob)
										(x, y)
										(fireDirection fire)
										(lineOfFire p)
										fireAction
										b
	where
	fireAction toReceive
			| newHP > 0 	= Just $ toReceive { robotHitpoints = newHP }
			| otherwise		= Nothing
		where newHP = robotHitpoints toReceive - hitpointsRemoved p (materialExpended fire)
getAction _ ((x, y, _), Dig) b
		| mat == SpotMaterial		= updateSpot (x,y) SpotEmpty b
		| otherwise					= b
		where GameSpot mat _ = unpack $ b !!! (x, y)
getAction _ ((x, y, rob), MoveIn dir) b
									= setRobot (x, y, Nothing) $ setRobot (newx, newy, Just rob) b
	where (newx, newy) = applyOffset (getOffset (robotTeam rob) dir) (x, y)
getAction params ((x, y, rob), spawn@(Spawn _ _ _ _ _)) b
									= setRobot (newx, newy, Just newRobot) b
	where
	newRobot = Robot {
		robotProgram = newProgram spawn,
		robotTeam = robotTeam rob,
		robotAppearance = newAppearance spawn,
		robotMaterial = paramInitialMaterial params,
		robotHitpoints = paramInitialHP params,
		robotBirthdate = boardTime b,
		robotMemory = newMemory spawn,
		robotMessages = []
	}
	(newx, newy) = applyOffset (getOffset (robotTeam rob) $ newDirection spawn) (x, y)

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
			Just (_, _, toReceive) 	-> setRobot (x, y, mutator toReceive) b
			Nothing					-> b
	where
	maybeRobot = robotAlongPath team b (x, y) direction distance

--	Removes any moves that would result in two robots being in the same spot.
--	Whichever move is a movement will be removed. If both are movements, both
 	-- are removed
removeCompetingMoves :: [RobotAndAction] -> [RobotAndAction]
removeCompetingMoves [] = []
removeCompetingMoves (move:remainder)
		| works			= move : pureRest
		| otherwise 	= pureRest
	where
	(works, rest) = removeCompetitionTo move remainder
	pureRest = removeCompetingMoves rest
	-- Removes all moves that compete with the given move.
	-- Outputs False if the currently processed move should be removed, True otherwise
	removeCompetitionTo :: RobotAndAction -> [RobotAndAction] -> (Bool, [RobotAndAction])
	removeCompetitionTo current@((xcur,ycur,_), _) other
			| any (fst . fault) robAndLocOther			= (False, other)
			| otherwise									= (True, map fst $ nonConflicting)
		where
		nonConflicting :: [(RobotAndAction, (Int, Int))]
		nonConflicting = filter (not . snd . fault) robAndLocOther
		locThis :: [(Int, Int)]
		locThis = finalLocations current
		robAndLocOther :: [(RobotAndAction, (Int, Int))]
		robAndLocOther
			= concat $ map (\(raa, xys) -> map (raa,) xys) robAsscLocs
			where
			robAsscLocs = zip other $ map finalLocations other
		fault :: (RobotAndAction, (Int, Int)) -> Conflict
		fault (_, xy) = (any (==xy) locThis, xy == (xcur, ycur))
		finalLocations :: RobotAndAction -> [(Int, Int)]
		finalLocations ((x,y,rob), act) = trace ("Final locations for " ++ printRobotAndAction ((x, y, rob), act) ++ "\n\t" ++ show (locs act)) $ locs act
			where
			locs (MoveIn dir) = [applyOffset (getOffset (robotTeam rob) dir) (x,y)]
			locs spawn@(Spawn _ _ _ _ _)
				= [applyOffset (getOffset (robotTeam rob) $ newDirection spawn) (x,y)]
			locs Die = []
			locs _ = [(x,y)]

-- first one is if my fault, second is if other fault
type Conflict = (Bool, Bool)

-- Takes a list of robots and results and outputs
	-- (a function that updates a board to one with hard drives updated,
	-- 	a list of robots and their actions)
-- This is performed before any robot actions are carried out.
updateAllHardDrives :: [RobotAndResult] -> (Board -> Board, [RobotAndAction])
updateAllHardDrives rars = (
		foldr (.) id $ map updateHardDrive rars,
		map removeState rars)
	where
	removeState (xyrob, (act, _)) = (xyrob, act)
	updateHardDrive ((x,y,rob), (_, state))
		= setRobot (x,y,Just rob {robotMemory = state})

-- Takes a parameter list and list of actions, and applies all their costs to
-- the given board.
applyActionCosts :: Parameters -> [RobotAndAction] -> Board -> Board
applyActionCosts params raas = foldr (.) id $ map applyActionCost raas
	where
	applyActionCost :: RobotAndAction -> Board -> Board
	applyActionCost ((x,y,rob), act) = setRobot (x, y, Just newRobot)
		where
		cost = actionCost params act
		newRobot = rob {robotMaterial = robotMaterial rob - cost}

-- Gets a list of robots and their program results
getRobotResults :: Parameters -> Board -> [RobotAndResult]
getRobotResults p b = map getRobotResult $ boardRobots b
	where
	getRobotResult (x, y, rob) = ((x, y, rob), robotProgram rob state)
	   where state = getKnownState p (robotTeam rob) b (x, y, rob)

-- Gets the known state for the given robot
getKnownState :: Parameters -> Team -> Board -> (Int, Int, Robot) -> KnownState
getKnownState p team b (x, y, rob) = KnownState {
		peekAtSpot = peekFn,
		material = robotMaterial rob,
		stateLocation = (x,y),
		stateAge = boardTime b - robotBirthdate rob,
		stateMemory = robotMemory rob,
		stateMessages = robotMessages rob
	}
	where
	peekFn :: [RDirection] -> Maybe SeenSpot
	peekFn directs
		| withinRange
			= (b !!! (applyOffset (asSeen (robotTeam rob) offs) (x, y)))
				>>= Just . toSeenSpot
		| otherwise	= Nothing
	        where
			withinRange = squareNorm offs <= (lineOfSight p) * (lineOfSight p)
			offs = foldr (addOffset) (Offset 0, Offset 0) $ map (getOffset team) directs
-- Returns the closest approximation to the requested action that is possible
    -- given the robot's level of material
-- This may be another type of action.
possibleAction :: Parameters -> RobotAndAction -> RobotAndAction
possibleAction p (xyrob@(_, _, rob), action)
    | actionCost p action <= robotMaterial rob  = (xyrob, action)
    | otherwise 								= downgrade action
	where
	downgrade :: RobotAction -> RobotAndAction
	downgrade Die = (xyrob, Die)
	downgrade Noop = (xyrob, Die) -- no alternative
	downgrade (MoveIn _) = tryNoop
	downgrade Dig = tryNoop
	downgrade s@(Spawn _ _ _ _ _) -- TODO Potential massive inefficiency here!
		= possibleAction p (xyrob, s {newMaterial = newMaterial s - 1})
	downgrade f@(Fire _ _)
		= possibleAction p (xyrob, f {materialExpended = materialExpended f - 1})
	downgrade (SendMessage _ _) = tryNoop
	tryNoop = possibleAction p (xyrob, Noop)

-- Increments time by one
applyTimeTick :: Board -> Board
applyTimeTick b = b {boardTime = boardTime b + 1}
