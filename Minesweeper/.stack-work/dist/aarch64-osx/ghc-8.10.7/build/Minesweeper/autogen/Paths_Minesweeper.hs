{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Minesweeper (
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

bindir     = "/Users/louie/College/FuncPro/Topics/Minesweeper/.stack-work/install/aarch64-osx/da08e9ba388a13369362e6dada0d3949b149585ec4dbd1fe28ee35eaea67e29f/8.10.7/bin"
libdir     = "/Users/louie/College/FuncPro/Topics/Minesweeper/.stack-work/install/aarch64-osx/da08e9ba388a13369362e6dada0d3949b149585ec4dbd1fe28ee35eaea67e29f/8.10.7/lib/aarch64-osx-ghc-8.10.7/Minesweeper-0.1.0.0-HFtZ6dkhIm03AwT1przx1g-Minesweeper"
dynlibdir  = "/Users/louie/College/FuncPro/Topics/Minesweeper/.stack-work/install/aarch64-osx/da08e9ba388a13369362e6dada0d3949b149585ec4dbd1fe28ee35eaea67e29f/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/louie/College/FuncPro/Topics/Minesweeper/.stack-work/install/aarch64-osx/da08e9ba388a13369362e6dada0d3949b149585ec4dbd1fe28ee35eaea67e29f/8.10.7/share/aarch64-osx-ghc-8.10.7/Minesweeper-0.1.0.0"
libexecdir = "/Users/louie/College/FuncPro/Topics/Minesweeper/.stack-work/install/aarch64-osx/da08e9ba388a13369362e6dada0d3949b149585ec4dbd1fe28ee35eaea67e29f/8.10.7/libexec/aarch64-osx-ghc-8.10.7/Minesweeper-0.1.0.0"
sysconfdir = "/Users/louie/College/FuncPro/Topics/Minesweeper/.stack-work/install/aarch64-osx/da08e9ba388a13369362e6dada0d3949b149585ec4dbd1fe28ee35eaea67e29f/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Minesweeper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Minesweeper_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Minesweeper_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Minesweeper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Minesweeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Minesweeper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
