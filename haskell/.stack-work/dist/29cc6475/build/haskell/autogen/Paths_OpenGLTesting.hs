{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_OpenGLTesting (
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

bindir     = "S:\\Projects\\OpenGLTesting\\OpenGLTesting\\haskell\\.stack-work\\install\\bd6f2258\\bin"
libdir     = "S:\\Projects\\OpenGLTesting\\OpenGLTesting\\haskell\\.stack-work\\install\\bd6f2258\\lib\\x86_64-windows-ghc-8.8.3\\OpenGLTesting-0.1.0.0-7C4Gxih3B8C1VPbMYgyz07-haskell"
dynlibdir  = "S:\\Projects\\OpenGLTesting\\OpenGLTesting\\haskell\\.stack-work\\install\\bd6f2258\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "S:\\Projects\\OpenGLTesting\\OpenGLTesting\\haskell\\.stack-work\\install\\bd6f2258\\share\\x86_64-windows-ghc-8.8.3\\OpenGLTesting-0.1.0.0"
libexecdir = "S:\\Projects\\OpenGLTesting\\OpenGLTesting\\haskell\\.stack-work\\install\\bd6f2258\\libexec\\x86_64-windows-ghc-8.8.3\\OpenGLTesting-0.1.0.0"
sysconfdir = "S:\\Projects\\OpenGLTesting\\OpenGLTesting\\haskell\\.stack-work\\install\\bd6f2258\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "OpenGLTesting_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OpenGLTesting_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "OpenGLTesting_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "OpenGLTesting_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OpenGLTesting_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OpenGLTesting_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
