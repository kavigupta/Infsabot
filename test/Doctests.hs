{-# LANGUAGE DoAndIfThenElse #-}
module Doctests where

import Infsabot.Tools.Logic
import Test.DocTest

main :: IO ()
main =  getAll "Infsabot" >>= doctest
