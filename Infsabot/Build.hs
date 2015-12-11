{-# LANGUAGE DoAndIfThenElse #-}
module Infsabot.Build(main) where

import System.Directory(createDirectoryIfMissing, getDirectoryContents, doesDirectoryExist)
import System.Environment(getArgs)

import Data.List(isSuffixOf, intercalate)

import System.Exit
import System.Process(system, readProcessWithExitCode)
import Text.Regex.Posix((=~))

import Data.Functor
import Control.Applicative

import Test.HUnit(runTestTT, failures)

import Infsabot.Demoes(demoes)
import Infsabot.Test.CollatedTests(tests, stressTest, checks)

import Control.Monad(forM, when)

stdLog, errLog, errColor, logColor, sucColor, noColor, directory :: String

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
    when (performBuild whatToDo) buildAll
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

buildAll :: IO ()
buildAll
    = do
        contents <- getAll directory
        let hss = filter ((&&) <$> isSuffixOf ".hs" <*> not . isSuffixOf "Build.hs") contents
        code <- system "ghc -fno-code -fno-warn-orphans Infsabot/Build.hs -Werror"
        when (code /= ExitSuccess) $ echf "Failure in building Build"
        build hss
        echs "Haskell Files Built Correctly"
    where
    getAll :: FilePath -> IO [FilePath]
    getAll path =
        do
            direxists <- doesDirectoryExist path
            if not direxists then
                do
                    return [path]
            else
                do
                    allcontents <- getDirectoryContents path
                    let contents = map (\x -> path ++ "/" ++ x) $
                            filter
                                (\x -> not (isSuffixOf x ".") && not (isSuffixOf x ".."))
                                allcontents
                    subs <- forM contents $ getAll
                    return . concat $ subs
    build :: [String] -> IO ()
    build names =
        do
            let spacesep = intercalate " " names
            let dump = " >> " ++ stdLog ++ " 2> " ++ errLog
            echl $ "Compiling\n\t" ++ (intercalate "\n\t" names)
            let ghc = "ghc -fno-code -Wall -fno-warn-orphans -Werror -odir bin " ++ spacesep ++ dump
            ghcCode <- system ghc
            when (ghcCode /= ExitSuccess) $ echf "Error in compilation" >> pErrorClean
            (_,lintResult1, lintResult2) <- readProcessWithExitCode "hlint" names ""
            let lintResult = lintResult1 ++ lintResult2
            putStrLn $ show lintResult
            if lintResult =~ "([0-9]+)\\s+suggestions?" then do
                echf $ "Hlint suggestions ==>\n" ++ lintResult
                exitFailure
            else return ()

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
