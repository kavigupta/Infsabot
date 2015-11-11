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
    performChecks :: Bool,
    performTests :: Bool
} deriving(Show)

defaultWhatToDo = WhatToDo {
    performCommit = Nothing,
    performDemo=False,
    performClean=False,
    performChecks=True,
    performTests=True
}

main = do
    whatToDo <- analyzeArguments
    if performClean whatToDo then cleanUp else return ()
    createDirectoryIfMissing True "gen"
    createDirectoryIfMissing True "demo"
    buildAll
    echl "Running Quick Checks"
    if performChecks whatToDo then checks else return ()
    echs "Checks completed!"
    if performTests whatToDo then runTests else return ()
    if performDemo whatToDo then demoes else return ()
    commit $ performCommit whatToDo

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
        case enforceConsistency (readWhatToDo args) of
            Left err ->     echf err >>= const exitFailure
            Right wtd ->    return wtd

enforceConsistency :: Either String WhatToDo -> Either String WhatToDo
enforceConsistency (Left err) = Left err
enforceConsistency (Right wtd)
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
readWhatToDo ("-nochecks":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performChecks=False})
readWhatToDo ("-notests":rest)
    = readWhatToDo rest >>= \wtd -> return (wtd {performTests=False})
readWhatToDo (unrec:rest) = Left $ "Unrecognized command " ++ show unrec

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
