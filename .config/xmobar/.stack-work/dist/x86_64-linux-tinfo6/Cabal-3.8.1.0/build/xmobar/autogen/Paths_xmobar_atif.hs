{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_xmobar_atif (
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
version = Version [0,16] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/jk/.config/xmobar/.stack-work/install/x86_64-linux-tinfo6/e8537072e8b0c944866226c273fea02c5d5cf44043eb9655497edbfc0149d870/9.4.7/bin"
libdir     = "/home/jk/.config/xmobar/.stack-work/install/x86_64-linux-tinfo6/e8537072e8b0c944866226c273fea02c5d5cf44043eb9655497edbfc0149d870/9.4.7/lib/x86_64-linux-ghc-9.4.7/xmobar-atif-0.16-6xRAo6zdU6PGO46r78dihE-xmobar"
dynlibdir  = "/home/jk/.config/xmobar/.stack-work/install/x86_64-linux-tinfo6/e8537072e8b0c944866226c273fea02c5d5cf44043eb9655497edbfc0149d870/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/jk/.config/xmobar/.stack-work/install/x86_64-linux-tinfo6/e8537072e8b0c944866226c273fea02c5d5cf44043eb9655497edbfc0149d870/9.4.7/share/x86_64-linux-ghc-9.4.7/xmobar-atif-0.16"
libexecdir = "/home/jk/.config/xmobar/.stack-work/install/x86_64-linux-tinfo6/e8537072e8b0c944866226c273fea02c5d5cf44043eb9655497edbfc0149d870/9.4.7/libexec/x86_64-linux-ghc-9.4.7/xmobar-atif-0.16"
sysconfdir = "/home/jk/.config/xmobar/.stack-work/install/x86_64-linux-tinfo6/e8537072e8b0c944866226c273fea02c5d5cf44043eb9655497edbfc0149d870/9.4.7/etc"

getBinDir     = catchIO (getEnv "xmobar_atif_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "xmobar_atif_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "xmobar_atif_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "xmobar_atif_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmobar_atif_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmobar_atif_sysconfdir") (\_ -> return sysconfdir)




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
