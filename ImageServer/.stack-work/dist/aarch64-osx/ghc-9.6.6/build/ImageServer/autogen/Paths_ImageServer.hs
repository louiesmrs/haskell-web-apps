{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ImageServer (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/louie/College/FuncPro/Topics/ImageServer/.stack-work/install/aarch64-osx/d6149b2f682c8817c9440e9fab6b3275ae4f90bce9f4740299098c1f2d249384/9.6.6/bin"
libdir     = "/Users/louie/College/FuncPro/Topics/ImageServer/.stack-work/install/aarch64-osx/d6149b2f682c8817c9440e9fab6b3275ae4f90bce9f4740299098c1f2d249384/9.6.6/lib/aarch64-osx-ghc-9.6.6/ImageServer-0.1.0.0-FZp2ij1sKumLdbxIs6qqg5-ImageServer"
dynlibdir  = "/Users/louie/College/FuncPro/Topics/ImageServer/.stack-work/install/aarch64-osx/d6149b2f682c8817c9440e9fab6b3275ae4f90bce9f4740299098c1f2d249384/9.6.6/lib/aarch64-osx-ghc-9.6.6"
datadir    = "/Users/louie/College/FuncPro/Topics/ImageServer/.stack-work/install/aarch64-osx/d6149b2f682c8817c9440e9fab6b3275ae4f90bce9f4740299098c1f2d249384/9.6.6/share/aarch64-osx-ghc-9.6.6/ImageServer-0.1.0.0"
libexecdir = "/Users/louie/College/FuncPro/Topics/ImageServer/.stack-work/install/aarch64-osx/d6149b2f682c8817c9440e9fab6b3275ae4f90bce9f4740299098c1f2d249384/9.6.6/libexec/aarch64-osx-ghc-9.6.6/ImageServer-0.1.0.0"
sysconfdir = "/Users/louie/College/FuncPro/Topics/ImageServer/.stack-work/install/aarch64-osx/d6149b2f682c8817c9440e9fab6b3275ae4f90bce9f4740299098c1f2d249384/9.6.6/etc"

getBinDir     = catchIO (getEnv "ImageServer_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ImageServer_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ImageServer_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ImageServer_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ImageServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ImageServer_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
