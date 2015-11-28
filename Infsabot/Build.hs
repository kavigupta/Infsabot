module Infsabot.Build(main) where

import System.Directory(createDirectoryIfMissing, getDirectoryContents)
import System.Environment(getArgs)
import System.IO(readFile)

import Data.List(elemIndex, isSuffixOf)

import System.Exit
import System.Process(system)

import Test.HUnit(runTestTT, failures)

import Infsabot.Demoes(demoes)
import Infsabot.Tests(tests, stressTest)
import Infsabot.QuickChecks(checks)

import Control.Monad(forM_, when)

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
    performChecks :: Bool,
    performTests :: Bool,
    performStressTest :: Bool,
    performBuild :: Bool
} deriving(Show)

defaultWhatToDo = WhatToDo {
    performCommit = Nothing,
    performDemo=False,
    performClean=False,
    performChecks=True,
    performTests=True,
    performStressTest=False,
    performBuild=True
}

main = do
    whatToDo <- analyzeArguments
    when (performClean whatToDo) cleanUp
    createDirectoryIfMissing True "gen"
    createDirectoryIfMissing True "demo"
    when (performBuild whatToDo) buildAll
    echl "Running Quick Checks"
    when (performChecks whatToDo) checks
    echs "Checks completed!"
    when (performTests whatToDo) runTests
    when (performDemo whatToDo) demoes
    commit $ performCommit whatToDo
    when (performStressTest whatToDo) stressTest

cleanUp :: IO ()
cleanUp =
    do
        system "rm -r demo"
        system "rm -r gen"
        system "rm -r bin"
        -- remove deleted files from git
        system "git ls-files --deleted -z | xargs -0 git rm --ignore-unmatch"
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

analyzeArguments :: IO WhatToDo
analyzeArguments
    = do
        args <- getArgs
        case readWhatToDo args >>= enforceConsistency of
            Left err ->     echf err >>= const exitFailure
            Right wtd ->    return wtd

enforceConsistency :: WhatToDo -> Either String WhatToDo
enforceConsistency wtd
    | performCommit wtd /= Nothing =
        if not (performClean wtd)
            && performDemo wtd
            && performChecks wtd
            && performTests wtd
        then                    Right wtd
        else                    Left "Invalid argument combination"
    | otherwise = Right wtd
readWhatToDo :: [String] -> Either String WhatToDo
readWhatToDo [] = Right defaultWhatToDo
readWhatToDo ["-commit"] = Left "Commit requires an argument"
readWhatToDo ("-commit":msg:rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performCommit = Just msg})
readWhatToDo ("-demo":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performDemo=True})
readWhatToDo ("-clean":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performClean=True})
readWhatToDo ("-nobuild":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performBuild=False})
readWhatToDo ("-nochecks":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performChecks=False})
readWhatToDo ("-notests":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performTests=False})
readWhatToDo ("-stress":rest) = readWhatToDo rest >>= \wtd -> return (wtd {performStressTest=True})
readWhatToDo (unrec:_) = Left $ "Unrecognized command " ++ show unrec

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
