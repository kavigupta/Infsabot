module Infsabot.MathTools(isPrime) where

isPrime :: Int -> Bool
isPrime x
        | x < 2         =  False
        | otherwise     = not . or $ map (\f -> x `mod` f == 0) [2..(floor $ sqrt $ fromIntegral x)]
