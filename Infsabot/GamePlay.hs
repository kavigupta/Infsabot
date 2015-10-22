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

updateHardDrive :: (Int, Int, Robot) -> InternalState -> Board -> Board
updateHardDrive (x,y,rob) newstate = setRobot (x,y,rob {robotMemory = newstate})

applyNonModifyingChanges :: RobotAndResult -> Maybe (Board -> Board)
applyNonModifyingChanges ((x,y,rob), (Noop, newstate))
	= Just $ updateHardDrive (x,y,rob) newstate
applyNonModifyingChanges ((x,y,_), (Die, _))
	= Just $ deleteRobot (x,y)
applyNonModifyingChanges _ = Nothing

actionsInGroup :: [[RobotAndResult]] -> ActionGroup -> [RobotAndResult]
actionsInGroup rars ag = concat $ filter isInGroup rars
	where
	isInGroup :: [RobotAndResult] -> Bool
	isInGroup [] = False
	isInGroup ((_,(act,_)):_) = getActionGroup act == ag

groupedActions :: Board -> [[RobotAndResult]]
groupedActions b = groupBy ((==) `on` classify) getRobotActions
	where
	classify :: RobotAndResult -> ActionGroup
	classify (_, (act, _)) = getActionGroup act
	getRobotActions :: [RobotAndResult]
	getRobotActions = map (getRobotAction) $ boardRobots b
		where
		getRobotAction (x, y, rob) = ((x, y, rob), robotProgram rob state)
		   where state = getKnownState b (x, y, rob)

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
    -- given the robot's level of material.
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
