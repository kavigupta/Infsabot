module Infsabot.Strategy.StandardStrategies(
        randomMoves
    ) where

import Infsabot.Base.Interface
import Infsabot.Tools.Interface
import Infsabot.RobotAction.Interface

import Codec.Picture.Types(PixelRGB8(..))

import qualified Control.Monad.State as S
import System.Random

instance (Random RDirection) where
    random = S.runState $ do
        index <- S.state $ randomR (0, 3 :: Int)
        return $ [N, E, W, S] !! index
    randomR _ = random

instance (Random RobotAppearance) where
    random = S.runState $ RobotAppearance <$> S.liftM3 PixelRGB8 s s s
        where s = S.state random
    randomR _ = random

randomMoves :: [Double] -> StdGen -> RobotProgram
randomMoves distr startGen KnownState {stateMemory=state, material=mat}
        = (result, newState)
    where
    (result, state') = flip S.runState gen $ do
            r <- S.state $ randomR (0, 1)
            let moveType = fst . head . dropWhile ((< r) . snd) $ cDistr
            case moveType of
                0 -> return Noop
                1 -> Fire <$> (FireAction <$> (makeNatural <$> S.state (randomR (0, mat))) <*> S.state random)
                2 -> do
                    len <- S.state $ randomR (0, 40)
                    Send <$> (SendAction <$> S.replicateM len (S.state random) <*> S.state random)
                3 -> return Dig
                4 -> MoveIn <$> S.state random
                5 -> do
                        dir <- S.state random
                        app <- S.state random
                        newMat <- S.state $ randomR (0, mat-1)
                        newGen <- S.state split
                        return . Spawn $ SpawnAction
                            dir
                            (randomMoves distr gen)
                            app
                            (makeNatural newMat)
                            (insert state "gen" (show newGen))
                _ -> error "Doesn't work"
    cDistr :: [(Int, Double)]
    cDistr = zip [0..] . tail . scanl (+) 0 $ distr
    gen = case get state "gen" of
        Nothing -> startGen
        Just x -> read x
    newState = insert state "gen" (show state')
