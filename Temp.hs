import Infsabot.Board
import Infsabot.Base
import Infsabot.TestLibrary
import Infsabot.Robot
import Infsabot.RobotStrategy
import Infsabot.RobotAction
import Data.Map(fromList)
import Codec.Picture
import Infsabot.MoveConflictResolution
import Infsabot.Debug
import Infsabot.QuickChecks
import Test.QuickCheck.Property(exhaustive)

tests :: [RobotAndAction]
tests = [((33,2,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 18 75 15}, robotMaterial = 19, robotHitpoints = 0, robotBirthdate = 10, robotMemory = fromList [], robotMessages = []}),MoveIn E),((34,2,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 227 112 174}, robotMaterial = 4, robotHitpoints = 22, robotBirthdate = 33, robotMemory = fromList [], robotMessages = []}),MoveIn W),((34,1,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 130 50 122}, robotMaterial = 31, robotHitpoints = 8, robotBirthdate = 11, robotMemory = fromList [], robotMessages = []}),Dig)]

--makeSymmetric [((0,-2,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 1 0 0}, robotMaterial = 3, robotHitpoints = 4, robotBirthdate = 4, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 2 1 2}, newMaterial = 2, newMemory = fromList []}),((0,-1,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 2 2}, robotMaterial = 3, robotHitpoints = 2, robotBirthdate = 3, robotMemory = fromList [], robotMessages = []}),MoveIn W)]


--makeSymmetric [((-50,1,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 66 168 195}, robotMaterial = 63, robotHitpoints = 63, robotBirthdate = 41, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 159 139 144}, newMaterial = 41, newMemory = fromList []}),((-50,2,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 195 63}, robotMaterial = 61, robotHitpoints = 3, robotBirthdate = 9, robotMemory = fromList [], robotMessages = []}),MoveIn N)]

{-makeSymmetric [
	((-1,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 2 3}, robotMaterial = 3, robotHitpoints = 4, robotBirthdate = 4, robotMemory = fromList [], robotMessages = []}),Fire {fireDirection = W, materialExpended = -1}),
	((-1,7,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 2 1}, robotMaterial = 4, robotHitpoints = 5, robotBirthdate = 5, robotMemory = fromList [], robotMessages = []}),MoveIn S),
	((-2,7,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 7 7 8}, robotMaterial = 5, robotHitpoints = 2, robotBirthdate = 8, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = S, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 6 7 7}, newMaterial = -4, newMemory = fromList []})
	]
-}

{-
	[((-3,19,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 160 148 172}, robotMaterial = 36, robotHitpoints = 21, robotBirthdate = 14, robotMemory = fromList [], robotMessages = []}),MoveIn S),((-2,19,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 36 49 200}, robotMaterial = 25, robotHitpoints = 17, robotBirthdate = 1, robotMemory = fromList [], robotMessages = []}),Noop),((-3,18,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 43 202 119}, robotMaterial = 24, robotHitpoints = 26, robotBirthdate = 8, robotMemory = fromList [], robotMessages = []}),MoveIn E)]
-}

{-[((-12,45,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 61 19 106}, robotMaterial = 29, robotHitpoints = 40, robotBirthdate = 62, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 35 173 225}, newMaterial = 5, newMemory = fromList []}),((-11,45,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 198 148 144}, robotMaterial = 6, robotHitpoints = 34, robotBirthdate = 34, robotMemory = fromList [], robotMessages = []}),MoveIn W)]

-}


{-}[
	((9,21,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 400, robotHitpoints = 100, robotBirthdate = 2, robotMemory = fromList [], robotMessages = []}),MoveIn E),
	((21,9,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 400, robotHitpoints = 100, robotBirthdate = 2, robotMemory = fromList [], robotMessages = []}),MoveIn E),
	((9,20,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 996, robotHitpoints = 100, robotBirthdate = 19, robotMemory = fromList [], robotMessages = []}),MoveIn N),
    ((20,9,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 996, robotHitpoints = 100, robotBirthdate = 19, robotMemory = fromList [], robotMessages = []}),MoveIn N),
	((21,10,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 238, robotHitpoints = 100, robotBirthdate = 0, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, newMaterial = 79, newMemory = fromList []}),
	((10,21,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 238, robotHitpoints = 100, robotBirthdate = 0, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, newMaterial = 79, newMemory = fromList []}),
	((19,8,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 999, robotHitpoints = 100, robotBirthdate = 26, robotMemory = fromList [], robotMessages = []}),MoveIn E),
	((8,19,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 999, robotHitpoints = 100, robotBirthdate = 26, robotMemory = fromList [], robotMessages = []}),MoveIn E),
	--
	((7,19,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 408, robotHitpoints = 100, robotBirthdate = 16, robotMemory = fromList [], robotMessages = []}),MoveIn E),
	((19,7,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 408, robotHitpoints = 100, robotBirthdate = 16, robotMemory = fromList [], robotMessages = []}),MoveIn E)
    ]
-}


{-[
    ((1,3,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 635, robotHitpoints = 100, robotBirthdate = 1, robotMemory = fromList [], robotMessages = []}),MoveIn E),
    ((3,1,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 635, robotHitpoints = 100, robotBirthdate = 1, robotMemory = fromList [], robotMessages = []}),MoveIn E),
    ((2,3,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 0 0 128}, robotMaterial = 636, robotHitpoints = 100, robotBirthdate = 3, robotMemory = fromList [], robotMessages = []}),MoveIn E),
    ((3,2,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 255 0 0}, robotMaterial = 636, robotHitpoints = 100, robotBirthdate = 3, robotMemory = fromList [], robotMessages = []}),MoveIn E)]
-}
{-[((-52,0,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 94 250 23}, robotMaterial = 35, robotHitpoints = 42, robotBirthdate = 48, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = E, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 42 190 236}, newMaterial = -68, newMemory = fromList []}),
    ((-52,-2,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 242 34 8}, robotMaterial = 30, robotHitpoints = 16, robotBirthdate = 39, robotMemory = fromList [], robotMessages = []}),Noop),
    ((-51,0,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 174 233 27}, robotMaterial = 13, robotHitpoints = 38, robotBirthdate = 43, robotMemory = fromList [], robotMessages = []}),MoveIn W)]
-}
