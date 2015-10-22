import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Parameters
import Infsabot.Base

type RobotAndResult = ((Int, Int, Robot), RobotProgramResult)

type RobotAndAction = ((Int, Int, Robot), RobotAction)

-- the main play function. This executes all robot actions and updates the board.
play :: Parameters -> Board -> Board
play p b =
		-- apply the time tick
		applyTimeTick .
		-- update all hard drives
		hardDriveUpdater .
		-- apply all action costs
		actionCostApplier .
		-- apply all die actions
		dieApplier $
		b
	where
		-- all robots and robot program results
		results = getRobotResults b
		-- actions := results - state.
		(hardDriveUpdater, actions) = updateAllHardDrives results
		actionCostApplier = applyActionCosts p actions
		nonNoopActions = removeNoops actions
		(dieApplier, _) = applyDies nonNoopActions

removeNoops :: [RobotAndAction] -> [RobotAndAction]
removeNoops = filter isNotNoop
	where
	isNotNoop (_, Noop) = False
	isNotNoop _ 		= True

applyDies :: [RobotAndAction] -> (Board -> Board, [RobotAndAction])
applyDies [] = (id, [])
applyDies (((x,y,_),Die):remActions)
		= ((deleteRobot (x, y)) . restFunction, restActions)
	where
		(restFunction, restActions) = applyDies remActions
applyDies (nonDie:remActions) = (restFunction, nonDie:restActions)
	where
		(restFunction, restActions) = applyDies remActions
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
		robotMessages = []
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

-- The distance the given robot can see
lineOfSight :: Robot -> Int
lineOfSight _ = 1

-- The distance the robot can fire
lineOfFire :: Robot -> Int
lineOfFire _ = 2

-- Increments time by one
applyTimeTick :: Board -> Board
applyTimeTick b = b {boardTime = boardTime b + 1}
