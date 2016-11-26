module Infsabot.Strategy.Random.Tests(
        randomChecks
    ) where
        
import Infsabot.Strategy.Random.Logic(cRandom, complexity)
import Infsabot.Strategy.ExprTree.Interface

import Test.QuickCheck hiding (shuffle)

import Infsabot.Tools.Interface(doChecks)

import Infsabot.RobotAction.Tests()

import System.Random


randomChecks :: [IO Result]
randomChecks = [
          putStrLn "complexity . cRandom for ExprBool" >> doChecks 5 propCRandomComplexBool
    ]

propCRandomComplexBool :: Int -> Int -> Property
propCRandomComplexBool gen complex = complex >= 1 ==> complex == complexity randomFor
    where
    randomFor :: ExprBool
    randomFor = fst $ cRandom complex (mkStdGen gen)