module Paths_RSA (
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
version = Version {versionBranch = [1,0,2], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/bin"
libdir     = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.2/RSA-1.0.2"
datadir    = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/share/x86_64-linux-ghc-7.8.2/RSA-1.0.2"
libexecdir = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/libexec"
sysconfdir = "/home/armored/protoLocal/protocol/tpm/mainline/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "RSA_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RSA_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "RSA_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RSA_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RSA_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
