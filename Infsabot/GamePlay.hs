import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Parameters
import Infsabot.Base
import Data.List(sortBy)
import Data.Function(on)

type RobotAndResult = ((Int, Int, Robot), RobotProgramResult)
type RobotAndAction = ((Int, Int, Robot), RobotAction)

-- the main play function. This executes all robot actions and updates the board.
play :: Parameters -> Board -> Board
play p b =
		-- apply move and spawn actions
		actionApplier .
		-- apply all action costs
		actionCostApplier .
		-- update all hard drives
		hardDriveUpdater .
		-- apply the time tick
		applyTimeTick $
		b
	where
		-- all robots and robot program results
		results = getRobotResults b
		-- actions := results - state.
		(hardDriveUpdater, actions) = updateAllHardDrives results
		resolvedAndSortedActions
			= sortBy (compare `on` (orderOfOperations . snd))
				$ removeCompetingMoves actions
		actionCostApplier = applyActionCosts p actions
		actionApplier = foldl (.) id $ map (applyDNSF p) resolvedAndSortedActions

applyDNSF :: Parameters -> RobotAndAction -> Board -> Board
applyDNSF _ ((x,y,_),Die) b 	= deleteRobot (x, y) b
applyDNSF _ (_, Noop) b		 	= b
applyDNSF _ (xyrob, send@(SendMessage _ _)) b
								= mutateRobot xyrob sendAction (sendDirection send) b
	where
	sendAction toReceive 		= Just $ toReceive { robotMessages = newMessage: robotMessages toReceive }
		where
		newMessage :: (String, Direction)
		newMessage = (messageToSend send, oppositeDirection $ sendDirection send)
applyDNSF _ (xyrob, fire@(Fire _ _)) b
								= mutateRobot xyrob fireAction (fireDirection fire) b
	where
	fireAction toReceive
			| newHP > 0 		= Just $ toReceive { robotHitpoints = newHP }
			| otherwise			= Nothing
		where newHP = robotHitpoints toReceive - hitpointsRemoved (materialExpended fire)
applyDNSF _ ((x, y, _), Dig) b
		| mat == SpotMaterial	= updateSpot (x,y) SpotEmpty b
		| otherwise				= b
		where GameSpot mat _ = b !!! (x, y)
applyDNSF _ ((x, y, rob), MoveIn dir) b
								= deleteRobot (x, y) $ setRobot (newx, newy, rob) b
	where (newx, newy) = applyOffset (getOffset dir) (x, y)
applyDNSF params ((x, y, rob), spawn@(Spawn _ _ _ _ _)) b
								= setRobot (newx, newy, newRobot) b
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
	(newx, newy) = applyOffset (getOffset $ newDirection spawn) (x, y)

mutateRobot :: (Int, Int, Robot) -> (Robot -> Maybe Robot) -> Direction -> Board -> Board
mutateRobot (x, y, rob) mutator direction = individualAction
	where
	individualAction b
			= case maybeRobot of
				Just (_, _, toReceive) 	->
					case (mutator toReceive) of
						Just newRobot 		-> setRobot (x,y,newRobot) b
						Nothing				-> deleteRobot (x, y) b
				Nothing					-> b
		where
		maybeRobot = robotAlongPath b (x, y) direction (lineOfMessageSending rob)

-- Given a robot and action, gets a list containing ((oldx, oldy), (newx, newy))
finalLocations :: RobotAndAction -> [(Int, Int)]
finalLocations ((x,y,_), act) = locs act
	where
	locs (MoveIn dir) = [applyOffset (getOffset dir) (x,y)]
	locs spawn@(Spawn _ _ _ _ _)
		= [applyOffset (getOffset $ newDirection spawn) (x,y)]
	locs Die = []
	locs _ = [(x,y)]

removeCompetingMoves :: [RobotAndAction] -> [RobotAndAction]
removeCompetingMoves [] = []
removeCompetingMoves (x:xs)
		| works			= x : pureRest
		| otherwise 	= pureRest
	where
	(works, rest) = competingMove x xs
	pureRest = removeCompetingMoves rest

-- Removes all moves that compete with the given move.
-- Outputs False if the currently processed move should be removed, True otherwise
competingMove :: RobotAndAction -> [RobotAndAction] -> (Bool, [RobotAndAction])
competingMove current other
	| any canConflict robAndLocOther			= (True, other)
	| otherwise									= (False, map fst $ nonConflicting)
		where
		nonConflicting :: [(RobotAndAction, (Int, Int))]
		nonConflicting = filter (not . canConflict) robAndLocOther
		locThis :: [(Int, Int)]
		locThis = finalLocations current
		robAndLocOther :: [(RobotAndAction, (Int, Int))]
		robAndLocOther = concat $ map (\(raa, xys) -> map (raa,) xys) robAsscLocs
			where
			robAsscLocs = zip other $ map finalLocations other
		canConflict :: (RobotAndAction, (Int, Int)) -> Bool
		canConflict (_, xy) = any (== xy) locThis
-- Takes a list of robots and results and outputs
	-- (a function that updates a board to one with hard drives updated,
	-- 	a list of robots and their actions)
-- This is performed before any robot actions are carried out.
updateAllHardDrives :: [RobotAndResult] -> (Board -> Board, [RobotAndAction])
updateAllHardDrives rars = (
		foldr (.) id $ map updateHardDrive rars,
		map removeState rars)
	where
	removeState ((x,y,rob), (act, _)) = ((x,y,rob), act)
	updateHardDrive ((x,y,rob), (_, state)) = setRobot (x,y,rob {robotMemory = state})

-- Takes a parameter list and list of actions, and applies all their costs to
-- the given board.
applyActionCosts :: Parameters -> [RobotAndAction] -> Board -> Board
applyActionCosts params raas = foldr (.) id $ map applyActionCost raas
	where
	applyActionCost :: RobotAndAction -> Board -> Board
	applyActionCost ((x,y,rob), act) = setRobot (x, y, newRobot)
		where
		cost = actionCost params act
		newRobot = rob {robotMaterial = robotMaterial rob - cost}

-- Gets a list of robots and their program results
getRobotResults :: Board -> [RobotAndResult]
getRobotResults b = map getRobotResult $ boardRobots b
	where
	getRobotResult (x, y, rob) = ((x, y, rob), robotProgram rob state)
	   where state = getKnownState b (x, y, rob)

-- Gets the known state for the given robot
getKnownState :: Board -> (Int, Int, Robot) -> KnownState
getKnownState b (x, y, rob) = KnownState {
		peekAtSpot = peekFn,
		material = robotMaterial rob,
		stateLocation = (x,y),
		stateAge = boardTime b - robotBirthdate rob,
		stateMemory = robotMemory rob,
		stateMessages = robotMessages rob
	}
	where
	peekFn :: Offset ->  Offset -> Maybe SeenSpot
	peekFn (Offset offx) (Offset offy)
		| withinRange	= Just $ toSeenSpot $ b !!! (x + offx, y + offy)
		| otherwise		= Nothing
            where withinRange = offx * offx + offy * offy <= (lineOfSight rob) * (lineOfSight rob)

-- Returns the closest approximation to the requested action that is possible
    -- given the robot's level of material
-- This may be another type of action.
possibleAction :: Parameters -> Robot -> RobotAction -> RobotAction
possibleAction p rob action
    | actionCost p action <= robotMaterial rob  = action
    | otherwise 								= downgrade action
	where
	downgrade :: RobotAction -> RobotAction
	downgrade Die = Die
	downgrade Noop = Die -- no alternative
	downgrade (MoveIn _) = tryNoop
	downgrade Dig = tryNoop
	downgrade s@(Spawn _ _ _ _ _) -- TODO Potential massive inefficiency here!
		= possibleAction p rob $ s {newMaterial = newMaterial s - 1}
	downgrade f@(Fire _ _)
		= possibleAction p rob $ f {materialExpended = materialExpended f - 1}
	downgrade (SendMessage _ _) = tryNoop
	tryNoop = possibleAction p rob Noop

hitpointsRemoved :: Int -> Int
hitpointsRemoved matExpend = 2 + matExpend

-- The distance the given robot can see
lineOfSight :: Robot -> Int
lineOfSight _ = 3

-- The distance the robot can fire
lineOfFire :: Robot -> Int
lineOfFire _ = 2

-- The distance the robot can fire
lineOfMessageSending :: Robot -> Int
lineOfMessageSending _ = 3

-- Increments time by one
applyTimeTick :: Board -> Board
applyTimeTick b = b {boardTime = boardTime b + 1}
