{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aeris (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anonymus-raccoon/projects/tek3/appdev/aeris/api/.stack-work/install/x86_64-linux-tinfo6/c7f8e1671b4c8b823703f9f418d8aa767b285db1ba0b44f1d1278e4627d57a06/8.10.7/bin"
libdir     = "/home/anonymus-raccoon/projects/tek3/appdev/aeris/api/.stack-work/install/x86_64-linux-tinfo6/c7f8e1671b4c8b823703f9f418d8aa767b285db1ba0b44f1d1278e4627d57a06/8.10.7/lib/x86_64-linux-ghc-8.10.7/aeris-0.1.0.0-Ii93pzEPVszJm4zcUYBZ8C-aeris-exe"
dynlibdir  = "/home/anonymus-raccoon/projects/tek3/appdev/aeris/api/.stack-work/install/x86_64-linux-tinfo6/c7f8e1671b4c8b823703f9f418d8aa767b285db1ba0b44f1d1278e4627d57a06/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/anonymus-raccoon/projects/tek3/appdev/aeris/api/.stack-work/install/x86_64-linux-tinfo6/c7f8e1671b4c8b823703f9f418d8aa767b285db1ba0b44f1d1278e4627d57a06/8.10.7/share/x86_64-linux-ghc-8.10.7/aeris-0.1.0.0"
libexecdir = "/home/anonymus-raccoon/projects/tek3/appdev/aeris/api/.stack-work/install/x86_64-linux-tinfo6/c7f8e1671b4c8b823703f9f418d8aa767b285db1ba0b44f1d1278e4627d57a06/8.10.7/libexec/x86_64-linux-ghc-8.10.7/aeris-0.1.0.0"
sysconfdir = "/home/anonymus-raccoon/projects/tek3/appdev/aeris/api/.stack-work/install/x86_64-linux-tinfo6/c7f8e1671b4c8b823703f9f418d8aa767b285db1ba0b44f1d1278e4627d57a06/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aeris_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aeris_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aeris_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aeris_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aeris_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aeris_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
