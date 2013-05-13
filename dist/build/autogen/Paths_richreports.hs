module Paths_richreports (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\bin"
libdir     = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\richreports-0.0.0.1\\ghc-7.4.2"
datadir    = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\richreports-0.0.0.1"
libexecdir = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\richreports-0.0.0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "richreports_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "richreports_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "richreports_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "richreports_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
