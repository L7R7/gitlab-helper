{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_gitlab_helper (
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
bindir     = "/home/leo/dev/gitlab-helper/.stack-work/install/x86_64-linux/8f33b39177101e707a24c967d60c96500cc18c35ab631bdef0eae1edf58d718f/9.4.8/bin"
libdir     = "/home/leo/dev/gitlab-helper/.stack-work/install/x86_64-linux/8f33b39177101e707a24c967d60c96500cc18c35ab631bdef0eae1edf58d718f/9.4.8/lib/x86_64-linux-ghc-9.4.8/gitlab-helper-0.1.0.0-DjCJoh6kAcFHxHynI7BRYD-gitlab-helper-exe"
dynlibdir  = "/home/leo/dev/gitlab-helper/.stack-work/install/x86_64-linux/8f33b39177101e707a24c967d60c96500cc18c35ab631bdef0eae1edf58d718f/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/leo/dev/gitlab-helper/.stack-work/install/x86_64-linux/8f33b39177101e707a24c967d60c96500cc18c35ab631bdef0eae1edf58d718f/9.4.8/share/x86_64-linux-ghc-9.4.8/gitlab-helper-0.1.0.0"
libexecdir = "/home/leo/dev/gitlab-helper/.stack-work/install/x86_64-linux/8f33b39177101e707a24c967d60c96500cc18c35ab631bdef0eae1edf58d718f/9.4.8/libexec/x86_64-linux-ghc-9.4.8/gitlab-helper-0.1.0.0"
sysconfdir = "/home/leo/dev/gitlab-helper/.stack-work/install/x86_64-linux/8f33b39177101e707a24c967d60c96500cc18c35ab631bdef0eae1edf58d718f/9.4.8/etc"

getBinDir     = catchIO (getEnv "gitlab_helper_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "gitlab_helper_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "gitlab_helper_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "gitlab_helper_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gitlab_helper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gitlab_helper_sysconfdir") (\_ -> return sysconfdir)




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
