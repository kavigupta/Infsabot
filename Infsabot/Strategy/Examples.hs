{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Strategy.GeneratedStrategy.Logic(
        digStrategy
    ) where

import Infsabot.RobotAction.Interface
import Infsabot.Base.Interface
import Infsabot.Tools.Interface(mod')

import Data.Maybe(fromMaybe, isJust)
import Data.Ratio
import Control.Applicative((<$>))
import System.Random


import Infsabot.Strategy.ExprTree.Interface

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
