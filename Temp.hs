import Infsabot.Board
import Infsabot.Base
import Infsabot.Robot
import Infsabot.RobotStrategy
import Infsabot.RobotAction
import Data.Map(fromList)
import Codec.Picture
import Infsabot.MoveConflictResolution
import Infsabot.Debug
import Infsabot.QuickChecks


tests :: [RobotAndAction]
tests = [

	((1,0,Robot {robotProgram = basicProgram B, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 50, robotHitpoints = 100, robotBirthdate = 0, robotMemory = fromList [], robotMessages = []}),
        MoveIn S),

	((0,1,Robot {robotProgram = basicProgram B, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 50, robotHitpoints = 100, robotBirthdate = 0, robotMemory = fromList [], robotMessages = []}),
        MoveIn E),

	((2,1,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 50, robotHitpoints = 100, robotBirthdate = 0, robotMemory = fromList [], robotMessages = []}),
        MoveIn N)]

otherTests :: [RobotAndAction]
otherTests = [
	((8,9,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, newMaterial = 333, newMemory = fromList []}),
	((9,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 1000, robotHitpoints = 100, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, newMaterial = 333, newMemory = fromList []}),
	((8,10,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = fromList [], robotMessages = []}),MoveIn N),
	((10,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 412, robotHitpoints = 100, robotBirthdate = 31, robotMemory = fromList [], robotMessages = []}),MoveIn N)
    ]

otter :: [(RobotAndAction, Bool)]
otter = map (\x -> (x, False)) otherTests
