module Paths_tpm (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/bin"
libdir     = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.2/tpm-0.1"
datadir    = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/share/x86_64-linux-ghc-7.8.2/tpm-0.1"
libexecdir = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/libexec"
sysconfdir = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tpm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tpm_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tpm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tpm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tpm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
