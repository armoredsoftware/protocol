module Paths_shell (
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
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/armored/.cabal/bin"
libdir     = "/home/armored/.cabal/lib/x86_64-linux-ghc-7.8.2/shell-1.0"
datadir    = "/home/armored/.cabal/share/x86_64-linux-ghc-7.8.2/shell-1.0"
libexecdir = "/home/armored/.cabal/libexec"
sysconfdir = "/home/armored/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "shell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
