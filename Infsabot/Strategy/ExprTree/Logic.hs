module Infsabot.Strategy.ExprTree.Logic(
        RP(..),
            ActionType(..),
            ExprInt(..),
            ExprPath(..),
            ExprDir(..),
            ExprBool(..),
    ) where

import Infsabot.Base.Interface

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
