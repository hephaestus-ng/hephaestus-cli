{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hephaestus_cli (
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

bindir     = "C:\\Users\\Pichau\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Pichau\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\hephaestus-cli-0.1.0.0-98dAkGj4Boz84Ws1mOp5vP"
dynlibdir  = "C:\\Users\\Pichau\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Pichau\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\hephaestus-cli-0.1.0.0"
libexecdir = "C:\\Users\\Pichau\\AppData\\Roaming\\cabal\\hephaestus-cli-0.1.0.0-98dAkGj4Boz84Ws1mOp5vP"
sysconfdir = "C:\\Users\\Pichau\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hephaestus_cli_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hephaestus_cli_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hephaestus_cli_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hephaestus_cli_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hephaestus_cli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hephaestus_cli_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
