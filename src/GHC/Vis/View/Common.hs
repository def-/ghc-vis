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
visState = unsafePerformIO $ newIORef $ State (0, 0) ListView 1 (0, 0) (0, 0) False

-- | All the visualized boxes
visBoxes :: MVar [(Box, String)]
visBoxes = unsafePerformIO (newMVar [] :: IO (MVar [(Box, String)]))

-- | Evaluate an object identified by a String.
evaluate :: String -> IO ()
evaluate identifier = do (_,hm) <- printAll
                         (show (map go hm) `deepseq` return ()) `catch`
                           \(e :: SomeException) -> putStrLn $ "Caught exception while evaluating: " ++ show e
  where go (Box a,(Just n, y)) | n == identifier = seq a (Just n, y)
                                 | otherwise = (Just n, y)
        go (_,(x,y)) = (x,y)

--printOne :: a -> IO String
--printOne a = do
--  bs <- readMVar visBoxes
--  case findIndex (\(b,_) -> asBox a == b) bs of
--    Just pos -> do
--      t  <- parseBoxes bs
--      return $ show (t !! pos)
--    Nothing -> return "Add entry first"

printAll :: IO (String, HeapMap)
printAll = do
  bs <- readMVar visBoxes
  (t, PState{heapMap = h}) <- parseBoxesHeap bs
  return (show t, h)
