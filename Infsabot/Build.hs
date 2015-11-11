module Infsabot.Build(main) where

import System.Directory(createDirectoryIfMissing, getDirectoryContents)
import System.Environment(getArgs)
import System.IO(readFile)

import Data.List(elemIndex, isSuffixOf)

import System.Exit
import System.Process(system)

import Test.HUnit(runTestTT, failures)

import Infsabot.Demoes(demoes)
import Infsabot.Tests(tests)
import Infsabot.QuickChecks(checks)
import Infsabot.Board(Board(..), renderBoard)
import Infsabot.Parameters

import Control.Monad(forM_)

import Codec.Picture(writePng)

demoBoardSize = 100

stdLog="gen/___std.log"
errLog="gen/___err.log"

errColor="\x1b[0;31m"
logColor="\x1b[0;32m"
sucColor="\x1b[1;36m"
noColor="\x1b[0m"

directory = "Infsabot"

data WhatToDo = WhatToDo {
    performCommit :: Maybe String,
    performDemo :: Bool,
    performClean :: Bool,
    noChecks :: Bool,
    noTests :: Bool
} deriving(Show)

main = do
    createDirectoryIfMissing True "gen"
    createDirectoryIfMissing True "demo"
    args <- getArgs
    let whatToDo = readWhatToDo args
    if performClean whatToDo then
        cleanUp
    else return ()
    buildAll
    echl "Running Quick Checks"
    checks
    echs "Checks completed!"
    runTests
    if performDemo whatToDo then demoes else return ()
    commit $ performCommit whatToDo

cleanUp :: IO ()
cleanUp =
    do
        system "rm -r demo"
        system "rm -r gen"
        system "rm -r bin"
        exitSuccess

runTests :: IO ()
runTests =
    do
        counts <- runTestTT tests
        if failures counts > 0 then
            do
                echf "Test Cases Failed"
                pErrorClean
        else
            echs "Test Cases Passed"

buildAll :: IO ()
buildAll
    = do
        contents <- getDirectoryContents directory
        forM_ (processHSs contents) build
        echs "Haskell Files Built Correctly"
    where
    processHSs :: [String] -> [String]
    processHSs paths = map ((directory ++ "/") ++)
        $ filter isHaskell paths
        where
        isHaskell s = isSuffixOf ".hs" s && s /= "Build.hs"
    build :: String -> IO ()
    build name =
        do
            echl $ "Compiling " ++ name
            exitCode <- system $ "ghc -fno-code -Wall -fno-warn-orphans -Werror -odir bin " ++ name
                ++ " >> " ++ stdLog
                ++ " 2> " ++ errLog
            case exitCode of
                (ExitFailure _) -> do
                    echf $ "Error compiling " ++ name
                    pErrorClean
                _ -> return ()

readWhatToDo :: [String] -> WhatToDo
readWhatToDo args
        = WhatToDo {
            performCommit = commit >>= (\x -> return $ args !! (x+1)),
            performClean = "-clean" `elem` args,
            performDemo = "-demo" `elem` args,
            noChecks = "-nochecks" `elem` args,
            noTests = "-notests" `elem` args
        }
    where
    commit :: Maybe Int
    commit = elemIndex "-commit" args

commit :: Maybe String -> IO ()
commit Nothing = return ()
commit (Just msg) =
    do
        putStrLn logColor
        system "git add ."
        system $ "git commit -m " ++ show msg
        system "git status"
        putStrLn noColor
        echs "Changes Committed"

echl :: String -> IO ()
echl s = putStrLn $ logColor ++ s ++ noColor
echs :: String -> IO ()
echs s = putStrLn $ sucColor ++ s ++ noColor
echf :: String -> IO ()
echf s = putStrLn $ errColor ++ s ++ noColor

pErrorClean =
    do
        putStrLn errColor
        cat errLog
        putStrLn noColor
        exitFailure

cat :: FilePath -> IO ()
cat path =
    do
        contents <- readFile path
        putStr contents
