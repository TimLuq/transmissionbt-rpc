module Paths_transmissionbt_rpc (
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
version = Version {versionBranch = [0,0,1,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/DATA/haskell/projects/transmissionbt-rpc/.cabal-sandbox/bin"
libdir     = "/media/DATA/haskell/projects/transmissionbt-rpc/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/transmissionbt-rpc-0.0.1.0"
datadir    = "/media/DATA/haskell/projects/transmissionbt-rpc/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/transmissionbt-rpc-0.0.1.0"
libexecdir = "/media/DATA/haskell/projects/transmissionbt-rpc/.cabal-sandbox/libexec"
sysconfdir = "/media/DATA/haskell/projects/transmissionbt-rpc/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "transmissionbt_rpc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "transmissionbt_rpc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "transmissionbt_rpc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "transmissionbt_rpc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "transmissionbt_rpc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
