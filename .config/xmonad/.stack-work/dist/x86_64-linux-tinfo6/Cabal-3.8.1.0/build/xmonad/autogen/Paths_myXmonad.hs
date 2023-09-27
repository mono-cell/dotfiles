{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_myXmonad (
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
bindir     = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/b4d4704c1aca572a849a3f60187452d5430ee83015b1db0daaa9389f0892733c/9.4.7/bin"
libdir     = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/b4d4704c1aca572a849a3f60187452d5430ee83015b1db0daaa9389f0892733c/9.4.7/lib/x86_64-linux-ghc-9.4.7/myXmonad-0.1.0.0-8NNnLzZvsDdESadr0W9O5A-xmonad"
dynlibdir  = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/b4d4704c1aca572a849a3f60187452d5430ee83015b1db0daaa9389f0892733c/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/b4d4704c1aca572a849a3f60187452d5430ee83015b1db0daaa9389f0892733c/9.4.7/share/x86_64-linux-ghc-9.4.7/myXmonad-0.1.0.0"
libexecdir = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/b4d4704c1aca572a849a3f60187452d5430ee83015b1db0daaa9389f0892733c/9.4.7/libexec/x86_64-linux-ghc-9.4.7/myXmonad-0.1.0.0"
sysconfdir = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/b4d4704c1aca572a849a3f60187452d5430ee83015b1db0daaa9389f0892733c/9.4.7/etc"

getBinDir     = catchIO (getEnv "myXmonad_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "myXmonad_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "myXmonad_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "myXmonad_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "myXmonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "myXmonad_sysconfdir") (\_ -> return sysconfdir)




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
