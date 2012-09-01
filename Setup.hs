module Main where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo

import System.FilePath

main = defaultMainWithHooks $
       simpleUserHooks { postInst = postInstHook (postInst simpleUserHooks) }

postInstHook oldHook args iflags pDesc lbi = do
  let instDataDir = datadir $ absoluteInstallDirs pDesc lbi (fromFlag $ copyDest defaultCopyFlags)
  putStrLn "To use ghc-vis you have to load its ghci file in GHCi. To do this automatically when GHCi is started run:"
  putStrLn $ "echo \":script " ++ (instDataDir </> "ghci") ++ "\" >> ~/.ghci"

  oldHook args iflags pDesc lbi
