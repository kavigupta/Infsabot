import Infsabot.Board
import Infsabot.Robot
import Infsabot.RobotAction
import Infsabot.Parameters
import Infsabot.Base
import Data.List(groupBy)
import Data.Function(on)

-- The distance the given robot can see
lineOfSight :: Robot -> Int
lineOfSight _ = 1

-- The distance the robot can fire
lineOfFire :: Robot -> Int
lineOfFire _ = 2

-- Increments time by one
applyTimeTick :: Board -> Board
applyTimeTick b = b {boardTime = boardTime b + 1}

type RobotAndResult = ((Int, Int, Robot), RobotProgramResult)

type RobotAndAction = ((Int, Int, Robot), RobotAction)

-- Takes a list of robots and results and outputs
	-- (a function that updates a board to one with hard drives updated,
	-- 	a list of robots and their actions)
-- This is performed before any robot actions are carried out.
updateAllHardDrives :: [RobotAndResult] -> (Board -> Board, [RobotAndAction])
updateAllHardDrives [] = (id, [])
updateAllHardDrives ((robAndCoor@(x,y,rob), (action, state)):rars)
		= (currentModifier . restOfmodifiers, (robAndCoor, action):restOfActions)
	where
	(restOfmodifiers, restOfActions) = updateAllHardDrives rars
	currentModifier b = setRobot (x,y,rob {robotMemory = state}) b


--applyNonExterMod :: RobotAndAction -> Maybe (Board -> Board)
--applyNonExterMod ((x,y,rob), Dig) b
--	| existMaterial == SpotEmpty	= Just id
--	where GameSpot existMaterial existingRobot = b !!! (x,y)

-- Gets the actions in the given group from the given list of actions.
-- May not be used in the future.
actionsInGroup :: [[RobotAndAction]] -> ActionGroup -> [RobotAndAction]
actionsInGroup rars ag = concat $ filter isInGroup rars
	where
	isInGroup :: [RobotAndAction] -> Bool
	isInGroup [] = False
	isInGroup ((_, act):_) = getActionGroup act == ag

-- Groups actions by their action execution group.
-- May not be used in the future
groupedActions :: [RobotAndAction] -> [[RobotAndAction]]
groupedActions = groupBy ((==) `on` classify)
	where
	classify :: RobotAndAction -> ActionGroup
	classify (_, act) = getActionGroup act

-- Gets a list of robots and their program results
getRobotActions :: Board -> [RobotAndResult]
getRobotActions b = map (getRobotAction) $ boardRobots b
	where
	getRobotAction (x, y, rob) = ((x, y, rob), robotProgram rob state)
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
	downgrade s@(Spawn _ _ _ _ _)
		= possibleAction p rob $ s {newMaterial = newMaterial s - 1}
	downgrade f@(Fire _ _)
		= possibleAction p rob $ f {materialExpended = materialExpended f - 1}
	downgrade (SendMessage _ _) = tryNoop
	tryNoop = possibleAction p rob Noop
