module Infsabot.Strategy.StandardStrategies(
        randomMoves,
        digger
    ) where

import Infsabot.Base.Interface
import Infsabot.Tools.Interface
import Infsabot.RobotAction.Interface

import Codec.Picture.Types(PixelRGB8(..))

import qualified Control.Monad.State as S
import System.Random

import Data.Maybe(fromMaybe)

instance (Random RDirection) where
    random = S.runState $ do
        index <- S.state $ randomR (0, 3 :: Int)
        return $ [N, E, W, S] !! index
    randomR _ = random

instance (Random RobotAppearance) where
    random = S.runState $ RobotAppearance <$> S.liftM3 PixelRGB8 s s s
        where s = S.state random
    randomR _ = random

digger :: RobotProgram -> RobotProgram
digger pro st@KnownState {stateMemory=state, peekAtSpot=peeker}
    = case peeker [] of
        Nothing -> (Noop, state)
        (Just (SeenSpot SpotEmpty _)) -> pro st
        (Just (SeenSpot SpotMaterial _)) -> (Dig, state)

randomMoves :: Maybe RobotProgram -> [Double] -> StdGen -> RobotProgram
randomMoves newPro distr startGen KnownState {stateMemory=state, material=mat}
        = (result, newState)
    where
    (result, state') = flip S.runState gen $ do
            r <- S.state $ randomR (0, 1)
            let moveType = fst . head . dropWhile ((< r) . snd) $ cDistr
            case moveType of
                0 -> return Noop
                1 -> return Dig
                2 -> MoveIn <$> S.state random
                3 -> do
                    dir <- S.state random
                    app <- S.state random
                    newMat <- S.state $ randomR (0, mat-1)
                    newGen <- S.state split
                    return . Spawn $ SpawnAction
                        dir
                        (fromMaybe (randomMoves newPro distr gen) newPro)
                        app
                        (makeNatural newMat)
                        (insert state "gen" (show newGen))
                4 -> Fire <$> (FireAction <$> (makeNatural <$> S.state (randomR (0, mat))) <*> S.state random)
                5 -> do
                    len <- S.state $ randomR (0, 40)
                    Send <$> (SendAction <$> S.replicateM len (S.state random) <*> S.state random)
                _ -> error "Doesn't work"
    cDistr :: [(Int, Double)]
    cDistr = zip [0..] . tail . scanl (+) 0 $ distr
    gen = case get state "gen" of
        Nothing -> startGen
        Just x -> read x
    newState = insert state "gen" (show state')
