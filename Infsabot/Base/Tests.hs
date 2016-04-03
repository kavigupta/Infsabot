{-# Language TemplateHaskell #-}
module Infsabot.Base.Tests (
    baseChecks
    ) where

import Infsabot.Base.Logic
import Data.Maybe
import Data.Function(on)
import Data.Functor
import Infsabot.Tools.Interface
import Codec.Picture (PixelRGB8(PixelRGB8))

import Data.DeriveTH(derive, makeArbitrary)
import Test.QuickCheck hiding (shuffle)

baseChecks :: [IO Result]
baseChecks = [
    putStrLn "Offset Is Limited" >> doChecks 1 propOffsetIsLimited,
    putStrLn "Linear Offset" >> doChecks 3 propLinearOffset]

propOffsetIsLimited :: Team -> Int -> [RDirection] -> (Int, Int) -> Bool
propOffsetIsLimited team len dirs (x, y)
        = not $ fromMaybe False value
    where
    final = limitedOffset team len dirs (x, y)
    value :: Maybe Bool
    value = (\(u, v) -> u * u + v * v > len * len) . sub (x, y) <$> final

propLinearOffset :: Team -> Int -> [RDirection] -> (Int, Int) -> (Int, Int) -> Bool
propLinearOffset team len dirs
        = (==) `on` off
    where
    off xy = sub xy <$> (limitedOffset team len dirs xy :: Maybe (Int, Int))

sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub (a, b) (c, d) = (a - c, b - d)

$( derive makeArbitrary ''PixelRGB8 )
$( derive makeArbitrary ''RobotAppearance )
$( derive makeArbitrary ''Team )
