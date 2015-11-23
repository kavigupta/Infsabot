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
tests =  [((-6,14,Robot {robotProgram = basicProgram A, robotTeam = A, robotAppearance = RobotAppearance {robotColor = PixelRGB8 19 4 9}, robotMaterial = 4, robotHitpoints = 9, robotBirthdate = 6, robotMemory = fromList [], robotMessages = []}),Noop),((-5,14,Robot {robotProgram = basicProgram A, robotTeam = B, robotAppearance = RobotAppearance {robotColor = PixelRGB8 5 26 16}, robotMaterial = 6, robotHitpoints = 11, robotBirthdate = 1, robotMemory = fromList [], robotMessages = []}),Spawn {newDirection = N, newProgram = basicProgram A, newAppearance = RobotAppearance {robotColor = PixelRGB8 11 32 25}, newMaterial = 11, newMemory = fromList []})]


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
