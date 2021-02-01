{-# LANGUAGE CPP, ScopedTypeVariables, RecordWildCards #-}
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
  visHidden,
  visHeapHistory,
  getHeapGraph,
  inHistoryMode,
  parseBoxes,
  parseBoxesHeap,
  evaluate
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Control.Concurrent
import Control.DeepSeq
import Control.Exception hiding (evaluate)

import Control.Monad.State.Strict hiding (State, fix)

import qualified Data.IntMap as M

import Data.IORef

import Data.Maybe (catMaybes)
import Data.List

import System.IO.Unsafe

import GHC.Vis.Internal
import GHC.Vis.Types
import GHC.HeapView

-- | Communication channel to the visualization
visSignal :: MVar Signal
{-# NOINLINE visSignal #-}
visSignal = unsafePerformIO (newEmptyMVar :: IO (MVar Signal))

-- | Whether a visualization is currently running
visRunning :: MVar Bool
{-# NOINLINE visRunning #-}
visRunning = unsafePerformIO (newMVar False)

defaultDepth :: Int
defaultDepth = 100

defaultView :: ViewType
#ifdef GRAPH_VIEW
defaultView = GraphView
#else
defaultView = ListView
#endif

-- | Internal state of the visualization
visState :: IORef State
{-# NOINLINE visState #-}
visState = unsafePerformIO $ newIORef $ State (0, 0) defaultView 1 (0, 0) False False defaultDepth

-- | All the visualized boxes
visBoxes :: MVar [NamedBox]
{-# NOINLINE visBoxes #-}
visBoxes = unsafePerformIO (newMVar [] :: IO (MVar [NamedBox]))

-- | Hidden boxes
visHidden :: MVar [Box]
{-# NOINLINE visHidden #-}
visHidden = unsafePerformIO (newMVar [] :: IO (MVar [Box]))

-- | All heap graphs since the last clear command
visHeapHistory :: MVar (Int, [(HeapGraph Identifier, [(Identifier, HeapGraphIndex)])])
{-# NOINLINE visHeapHistory #-}
visHeapHistory = unsafePerformIO (newMVar (0, [(HeapGraph M.empty, [])]) :: IO (MVar (Int, [(HeapGraph Identifier, [(Identifier, HeapGraphIndex)])])))

-- | Get the currently selected heap graph
getHeapGraph :: IO (HeapGraph Identifier, [(Identifier, HeapGraphIndex)])
getHeapGraph = do
  (pos, xs) <- readMVar visHeapHistory
  return $ xs !! pos

-- | Whether we're currently looking at an older heap graph or the most recent one
inHistoryMode :: IO Bool
inHistoryMode = liftM ((> 0) . fst) $ readMVar visHeapHistory

-- | Evaluate an object identified by a String.
evaluate :: String -> IO ()
evaluate identifier = do (_,HeapGraph m) <- printAll
                         (show (M.map go m) `deepseq` return ()) `catch`
                           \(e :: SomeException) -> putStrLn $ "Caught exception while evaluating: " ++ show e
  where go hge@(HeapGraphEntry (Box a) _ _ n) | n == identifier = seq a hge
                                              | otherwise = hge

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names.
parseBoxes :: IO [[VisObject]]
parseBoxes = generalParseBoxes evalState

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names. Also return the resulting 'HeapMap' and another
--   'HeapMap' that does not contain BCO pointers.
parseBoxesHeap :: IO ([[VisObject]], PState)
parseBoxesHeap = generalParseBoxes runState

--generalParseBoxes ::
--     (PrintState (Maybe [[VisObject]]) -> PState -> b)
--  -> [NamedBox] -> IO b
generalParseBoxes :: (PrintState [[VisObject]] -> PState -> b) -> IO b
generalParseBoxes f = do
  --(hg, starts) <- multiBuildHeapGraph 100 $ map (\(_,x) -> ("",x)) bs
  (hg@(HeapGraph m), starts) <- getHeapGraph
  let bindings = boundMultipleTimes hg $ map snd starts
  let g i = do
        r <- parseClosure i
        return $ simplify r
  return $ f (mapM (g . snd) starts) $ PState 1 1 1 bindings $ HeapGraph $ M.map (\hge -> hge{hgeData = ""}) m

-- | In the given HeapMap, list all indices that are used more than once. The
-- second parameter adds external references, commonly @[heapGraphRoot]@.
boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ group $ sort $
     roots ++ concatMap (catMaybes . allClosures . hgeClosure) (M.elems m)

-- Pulls together multiple Unnamed objects to one
simplify :: [VisObject] -> [VisObject]
simplify [] = []
simplify [Named a bs] = [Named a $ simplify bs]
simplify [a] = [a]
simplify (Unnamed a : Unnamed b : xs) = simplify $ Unnamed (a ++ b) : xs
simplify (Named a bs : xs) = Named a (simplify bs) : simplify xs
simplify (a:xs) = a : simplify xs

printAll :: IO (String, HeapGraph String)
printAll = do
  (t, PState{heapGraph = h}) <- parseBoxesHeap
  return (show t, h)
