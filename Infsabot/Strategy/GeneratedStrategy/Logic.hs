{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Strategy.GeneratedStrategy.Logic(
        RP(..),
            ActionType(..),
            ExprInt(..),
            ExprPath(..),
            ExprDir(..),
            ExprBool(..),
            digStrategy,
            eval
    ) where

import Infsabot.RobotAction.Interface
import Infsabot.Base.Interface
import Codec.Picture(PixelRGB8(..))

import Data.Maybe(fromMaybe, isJust)
import Data.Ratio
import Control.Applicative((<$>))
import Infsabot.Tools.Interface(mod')


data ActionType = TDie | TNoop | TFire | TDig | TMoveIn | TSpawn

data RP =
    DefaultRepresentation ActionType |
    ModifyMaterial ExprInt RP |
        -- Applies to Fire, Spawn
    ModifyDirection ExprDir RP |
        -- Applies to Move, Spawn, Fire, Send
    ModifyAppearance ExprInt RP |
        -- Applies to Spawn. RGB treated as a single integer.
    IfRP ExprBool RP RP

data ExprInt =
    ConstInt Int |
    RobAppear ExprPath ExprInt | -- Coordinate; Default if no robot
    MaterialLevel | -- Current material level
    Age | -- Current Age
    (:*) ExprInt ExprInt |
    (:+) ExprInt ExprInt |
    (:-) ExprInt ExprInt |
    (:/) ExprInt ExprInt |
    Mod ExprInt ExprInt |
    IfInt ExprBool ExprInt ExprInt

-- Basically [ExprDir]
    -- Written like this to make the tree more explicit
data ExprPath =
    Here |
    Offset ExprDir ExprPath

data ExprDir =
    ConstDir RDirection |
    IfDir ExprBool ExprDir ExprDir

data ExprBool =
    ConstBool Bool |
    CanSee ExprPath |
    MaterialAt ExprPath | -- False if can't see that far
    RobotAt ExprPath | -- False if can't see that far
    EqualInt ExprInt ExprInt |
    GreaterInt ExprInt ExprInt |
    EqualDir ExprDir ExprDir |
    Not ExprBool |
    (:&) ExprBool ExprBool |
    (:|) ExprBool ExprBool

class Evaluable given from to where
    eval :: given -> from -> to

evalIf :: (Evaluable a exb Bool, Evaluable a exo o)
        => a -> exb -> exo -> exo -> o
evalIf state cond ifso ifelse
    | eval state cond   = eval state ifso
    | otherwise         = eval state ifelse

exprPeek :: KnownState -> ExprPath -> Maybe SeenSpot
exprPeek state path = peekAtSpot state (eval state path :: [RDirection])

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

appearanceToInt :: RobotAppearance -> Int
appearanceToInt (RobotAppearance (PixelRGB8 r g b))
    = fromIntegral r * 256 * 256 + fromIntegral g * 256 + fromIntegral b

digStrategy :: RP
digStrategy
    = IfRP (MaterialAt Here)
        (DefaultRepresentation TDig)
        (ModifyDirection
            (IfDir
                (MaterialAt
                    (Offset (ConstDir N) Here))
                (ConstDir N)
                (IfDir
                    (MaterialAt
                        (Offset (ConstDir E) Here))
                    (ConstDir E)
                    (IfDir
                        (MaterialAt
                            (Offset (ConstDir S) Here))
                        (ConstDir S)
                        (ConstDir W))))
            (DefaultRepresentation TMoveIn))
