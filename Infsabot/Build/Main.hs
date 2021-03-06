{-# LANGUAGE DoAndIfThenElse #-}
module Main(main) where

import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)

import Data.List(isSuffixOf, intercalate)
import Data.Maybe(isJust)

import System.Exit
import System.Process(system, readProcessWithExitCode)
import Text.Regex.Posix((=~))

import Test.HUnit(runTestTT, failures)

import Infsabot.Demoes(demoes)
import Infsabot.Test.CollatedTests(tests, stressTest, checks)

import Control.Monad(when)
import Infsabot.Tools.Interface(getAll)

errLog, errColor, logColor, sucColor, noColor, directory :: String

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

defaultWhatToDo :: WhatToDo
defaultWhatToDo = WhatToDo {
    performCommit = Nothing,
    performDemo=False,
    performClean=False,
    performChecks=True,
    performTests=True,
    performStressTest=False,
    performBuild=True
}

main :: IO ()
main = do
    whatToDo <- analyzeArguments
    when (performClean whatToDo) cleanUp
    createDirectoryIfMissing True "gen"
    createDirectoryIfMissing True "demo"
    when (performBuild whatToDo) lintAll
    when (performChecks whatToDo) doChecks
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

doChecks :: IO ()
doChecks =
    do
        putStrLn $ logColor ++ "Running Checks"
        success <- checks
        if success then
            echs "QuickChecks passed!"
        else do
            echf "Quick Checks failed!"
            exitFailure

lintAll :: IO ()
lintAll = do
        contents <- getAll directory
        let hss = filter (isSuffixOf ".hs") contents
        lint hss
        echs "Haskell Files Built Correctly"
    where
    lint :: [String] -> IO ()
    lint names =
        do
            echl $ "Linting\n\t" ++ intercalate "\n\t" names
            (_,lintResult1, lintResult2) <- readProcessWithExitCode "hlint" names ""
            let lintResult = lintResult1 ++ lintResult2
            print lintResult
            when (lintResult =~ "([0-9]+)\\s+hints?") $ do
                echf $ "Hlint suggestions ==>\n" ++ lintResult
                exitFailure

analyzeArguments :: IO WhatToDo
analyzeArguments
    = do
        args <- getArgs
        case readWhatToDo args >>= enforceConsistency of
            Left err ->     echf err >>= const exitFailure
            Right wtd ->    return wtd

enforceConsistency :: WhatToDo -> Either String WhatToDo
enforceConsistency wtd
    | isJust $ performCommit wtd =
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

pErrorClean :: IO ()
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
