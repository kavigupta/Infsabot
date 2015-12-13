module Paths_Infsabot (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kavi/Dropbox/workspaces/Haskell/Infsabot/.cabal-sandbox/bin"
libdir     = "/home/kavi/Dropbox/workspaces/Haskell/Infsabot/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/Infsabot-0.1.0.0"
datadir    = "/home/kavi/Dropbox/workspaces/Haskell/Infsabot/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/Infsabot-0.1.0.0"
libexecdir = "/home/kavi/Dropbox/workspaces/Haskell/Infsabot/.cabal-sandbox/libexec"
sysconfdir = "/home/kavi/Dropbox/workspaces/Haskell/Infsabot/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Infsabot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Infsabot_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Infsabot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Infsabot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Infsabot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
