import GHC.HeapView
import System.Mem
import System.Mem.StableName
import System.IO.Unsafe
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

instance Ord Box
  where compare (Box x) (Box y) = unsafePerformIO $ (do
          a <- makeStableName x
          b <- makeStableName y
          return $ compare (hashStableName a) (hashStableName b)
          )

type HeapEntry = (Maybe String, Closure)
type HeapMap   = Map Box HeapEntry

walkHeap :: a -> IO HeapMap
walkHeap a = do
  --performGC
  go (asBox a) Map.empty
  where go b l = case Map.lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) (Map.insert b (Nothing, c') l) (allPtrs c')
                     return $ (Map.insert b (Nothing, c') l')

walkHeapDepth a d = do
  performGC
  go (asBox a) d Map.empty
  where go b d l | d == 0 = return l
                 | otherwise = case Map.lookup b l of
                     Just _  -> return l
                     Nothing -> do
                       c' <- getBoxedClosureData b
                       l' <- foldM (\l x -> go x (d - 1) l) (Map.insert b (Nothing, c') l) (allPtrs c')
                       return $ (Map.insert b (Nothing, c') l')

a x = 2 * x

--demoBCO x = do
--    a <- getClosureData x
--    b <- getBoxedClosureData $ fun a -- BCO
--    demoBCO2 b
--
--demoBCO2 b = do
--    c <- getBoxedClosureData $ bcoptrs b
--    ds <- mapM getBoxedClosureData (mccPayload c)
--    let e = head $ filter isBCO ds
--    putStrLn $ show e
--    demoBCO2 e
--  where isBCO (BCOClosure _ _ _ _ _ _ _) = True
--        isBCO _ = False
--
--getBCO b = do
--    c <- getBoxedClosureData $ bcoptrs b
--    ds <- mapM getBoxedClosureData (mccPayload c)
--    return $ head $ filter isBCO ds
--  where isBCO (BCOClosure _ _ _ _ _ _ _) = True
--        isBCO _ = False
