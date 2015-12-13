{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Infsabot.Strategy.Evaluable.Logic(
        eval
    ) where

import Infsabot.Strategy.ExprTree.Interface
import Infsabot.RobotAction.Interface
import Infsabot.Base.Interface
import Infsabot.Tools.Interface(mod')

import Data.Maybe(fromMaybe, isJust)
import Data.Ratio
import Control.Applicative((<$>))

import Codec.Picture(PixelRGB8(..))

class Evaluable given from to where
    eval :: given -> from -> to

instance Evaluable KnownState ExprInt (Ratio Int) where
    -- Evaluates to a Ratio, or fraction, for precision reasons.
    -- Round to get an actual integer
    eval _ (ConstInt x) = fromIntegral x
    eval state (RobAppear path def)
            = fromMaybe (eval state def) $
                fromIntegral . appearanceToInt <$> (exprPeek state path >>= getAppearance)
        where
        getAppearance (SeenSpot _ x) = x
    eval state MaterialLevel = fromIntegral $ material state
    eval state Age = fromIntegral $ stateAge state
    eval state (x :+ y) = eval state x + eval state y
    eval state (x :* y) = eval state x * eval state y
    eval state (x :- y) = eval state x - eval state y
    eval state (x :/ y) = eval state x / eval state y
    eval state (x `Mod` y) = eval state x `mod'` eval state y
    eval state (IfInt cond ifso ifelse) = evalIf state cond ifso ifelse

instance Evaluable KnownState ExprBool Bool where
    eval _ (ConstBool x) = x
    eval state (CanSee path) = isJust $ exprPeek state path
    eval state (MaterialAt path)
            = fromMaybe False $ getIsMaterial <$> exprPeek state path
        where getIsMaterial (SeenSpot mat _) = mat == SpotMaterial
    eval state (RobotAt path)
            = fromMaybe False $ getIsRobot <$> exprPeek state path
        where getIsRobot (SeenSpot _ r) = isJust r
    eval state (EqualInt x y)
        = (eval state x :: Ratio Int) == eval state y
    eval state (GreaterInt x y)
        = (eval state x :: Ratio Int) > eval state y
    eval state (EqualDir x y)
        = (eval state x :: RDirection) == eval state y
    eval state (Not x) = not $ eval state x
    eval state (x :& y) = eval state x && eval state y
    eval state (x :| y) = eval state x || eval state y

instance Evaluable KnownState ExprDir RDirection where
    eval _ (ConstDir x) = x
    eval state (IfDir cond ifso ifelse) = evalIf state cond ifso ifelse

instance Evaluable KnownState ExprPath [RDirection] where
    eval _ Here = []
    eval state (Offset dir path) = eval state dir : eval state path

evalIf :: (Evaluable a exb Bool, Evaluable a exo o)
        => a -> exb -> exo -> exo -> o
evalIf state cond ifso ifelse
    | eval state cond   = eval state ifso
    | otherwise         = eval state ifelse

exprPeek :: KnownState -> ExprPath -> Maybe SeenSpot
exprPeek state path = peekAtSpot state (eval state path :: [RDirection])

appearanceToInt :: RobotAppearance -> Int
appearanceToInt (RobotAppearance (PixelRGB8 r g b))
    = fromIntegral r * 256 * 256 + fromIntegral g * 256 + fromIntegral b
