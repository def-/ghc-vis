module Main where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription (PackageDescription)

import System.FilePath
import System.Info
import Data.List

main = defaultMainWithHooks $ simpleUserHooks
  { postInst = postInstHook (postInst simpleUserHooks)
  -- Copy for Cabal 1.20, which does not work with postInst
  , postCopy = postInstHook (postCopy simpleUserHooks)
  , postConf = postConfHook (postConf simpleUserHooks)
  }

postInstHook oldHook args iflags pDesc lbi = do
  let instDataDir = datadir $ absoluteInstallDirs pDesc lbi (fromFlag $ copyDest defaultCopyFlags)
  putStrLn "To use ghc-vis you have to load its ghci file in GHCi. To do this automatically when GHCi is started run:"
  case stripPrefix "mingw" os of
    Just _ -> putStrLn $ "echo \":script " ++ (instDataDir </> "ghci") ++ "\" >> $APPDATA/ghc/ghci.conf"
    Nothing -> putStrLn $ "echo \":script " ++ (instDataDir </> "ghci") ++ "\" >> ~/.ghci"

  oldHook args iflags pDesc lbi

postConfHook oldHook args flags descr buildInfo = case profFlag of
  Flag True -> error "This library cannot be built using profiling. Try invoking cabal with the --disable-library-profiling flag."
  _ -> oldHook args flags descr buildInfo
  where profFlag = configProfLib $ configFlags buildInfo
