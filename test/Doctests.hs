{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Control.Monad(forM_)
import Infsabot.Tools.Logic
import Test.DocTest

main :: IO ()
main = do
    sources <- getAll "Infsabot"
    forM_ sources $ \hs -> do
        putStrLn $ "Doctesting " ++ hs
        doctest [hs]
