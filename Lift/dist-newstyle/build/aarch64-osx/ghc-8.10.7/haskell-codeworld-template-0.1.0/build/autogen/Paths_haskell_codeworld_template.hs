{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_codeworld_template (
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
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/roman_molochkov/.cabal/bin"
libdir     = "/Users/roman_molochkov/.cabal/lib/aarch64-osx-ghc-8.10.7/haskell-codeworld-template-0.1.0-inplace"
dynlibdir  = "/Users/roman_molochkov/.cabal/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/roman_molochkov/.cabal/share/aarch64-osx-ghc-8.10.7/haskell-codeworld-template-0.1.0"
libexecdir = "/Users/roman_molochkov/.cabal/libexec/aarch64-osx-ghc-8.10.7/haskell-codeworld-template-0.1.0"
sysconfdir = "/Users/roman_molochkov/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_codeworld_template_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_codeworld_template_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_codeworld_template_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_codeworld_template_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_codeworld_template_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_codeworld_template_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
