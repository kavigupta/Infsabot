{-# Language TypeFamilies #-}
module Infsabot.Tools.Tests(toolsChecks) where

import Test.QuickCheck
import Infsabot.Tools.Logic


toolsChecks :: [IO Result]
toolsChecks = [
        doChecks 3 appliedSpanNeqWorks
    ]

appliedSpanNeqWorks :: [String] -> [String] -> [String] -> Bool
appliedSpanNeqWorks a b = propspanNeqWorks (`elem` a) (`elem` b)

propspanNeqWorks :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> Bool
propspanNeqWorks f filt xs
    = spanNeq f filt xs
        == let (a, b) = span f xs
            in (deleteFirst filt a, b)

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst f (x:xs)
    | f x           = x : deleteFirst f xs
    | otherwise     = xs

instance Arbitrary Natural where
    arbitrary = makeNatural <$> arbitrary
