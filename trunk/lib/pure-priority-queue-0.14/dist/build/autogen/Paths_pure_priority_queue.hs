module Paths_pure_priority_queue (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,14], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/pure-priority-queue-0.14/ghc-6.12.1"
datadir    = "/usr/local/share/pure-priority-queue-0.14"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "pure_priority_queue_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "pure_priority_queue_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "pure_priority_queue_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "pure_priority_queue_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
