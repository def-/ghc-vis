{-# LANGUAGE CPP, ScopedTypeVariables #-}
{- |
   Module      : GHC.Vis.View.Common
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.View.Common (
  visSignal,
  visRunning,
  visState,
  visBoxes,
  evaluate
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Control.Concurrent
import Control.DeepSeq
import Control.Exception hiding (evaluate)

import qualified Data.IntMap as M

import Data.IORef

import System.IO.Unsafe

import GHC.Vis.Internal
import GHC.Vis.Types
import GHC.HeapView

-- | Communication channel to the visualization
visSignal :: MVar Signal
visSignal = unsafePerformIO (newEmptyMVar :: IO (MVar Signal))

-- | Whether a visualization is currently running
visRunning :: MVar Bool
visRunning = unsafePerformIO (newMVar False)

-- | Internal state of the visualization
visState :: IORef State
visState = unsafePerformIO $ newIORef $ State (0, 0) ListView 1 (0, 0) False False

-- | All the visualized boxes
visBoxes :: MVar [NamedBox]
visBoxes = unsafePerformIO (newMVar [] :: IO (MVar [NamedBox]))

-- | Evaluate an object identified by a String.
evaluate :: String -> IO ()
evaluate identifier = do (_,HeapGraph m) <- printAll
                         (show (M.map go m) `deepseq` return ()) `catch`
                           \(e :: SomeException) -> putStrLn $ "Caught exception while evaluating: " ++ show e
  where go hge@(HeapGraphEntry (Box a) _ _ n) | n == identifier = seq a hge
                                              | otherwise = hge

printAll :: IO (String, HeapGraph String)
printAll = do
  bs <- readMVar visBoxes
  (t, PState{heapGraph = h}) <- parseBoxesHeap bs
  return (show t, h)
