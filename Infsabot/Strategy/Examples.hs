{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Infsabot.Strategy.Examples(
        digStrategy
    ) where

import Infsabot.Base.Interface

import Infsabot.Strategy.ExprTree.Interface
import Infsabot.Strategy.Random.Interface

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
