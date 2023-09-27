{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_config (
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
bindir     = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/fd1b58b8c12cfabd8242ce718af2800981648169db927c4163d244f2ab952ce0/9.2.5/bin"
libdir     = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/fd1b58b8c12cfabd8242ce718af2800981648169db927c4163d244f2ab952ce0/9.2.5/lib/x86_64-linux-ghc-9.2.5/config-0.1.0.0-A7f00aKiAOW9sS0gUZ84x8-xmobar-0"
dynlibdir  = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/fd1b58b8c12cfabd8242ce718af2800981648169db927c4163d244f2ab952ce0/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/fd1b58b8c12cfabd8242ce718af2800981648169db927c4163d244f2ab952ce0/9.2.5/share/x86_64-linux-ghc-9.2.5/config-0.1.0.0"
libexecdir = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/fd1b58b8c12cfabd8242ce718af2800981648169db927c4163d244f2ab952ce0/9.2.5/libexec/x86_64-linux-ghc-9.2.5/config-0.1.0.0"
sysconfdir = "/home/jk/.config/xmonad/.stack-work/install/x86_64-linux-tinfo6/fd1b58b8c12cfabd8242ce718af2800981648169db927c4163d244f2ab952ce0/9.2.5/etc"

getBinDir     = catchIO (getEnv "config_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "config_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "config_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "config_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "config_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "config_sysconfdir") (\_ -> return sysconfdir)




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
