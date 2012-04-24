{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.HeapView
import Control.Monad
import Data.Tree
import Data.Word
import GHC.Prim
import GHC.Exts

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap

buildTree x = do
  cd <- getBoxedClosureData x
  return (cd, allPtrs cd)

t x = unfoldTreeM buildTree x

u = t . asBox

-- infinite tree duh
f = let y = id (:) () y
    in getClosureData y

boxAddr :: Box -> Int
boxAddr (Box any) = fromIntegral $ W# (aToWord# any)

ins :: Box -> Closure -> IntMap Closure -> IntMap Closure
ins box closure im = IntMap.insert (boxAddr box) closure im

walkHeap a = go (asBox a) IntMap.empty
  where go b im | (boxAddr b) `IntMap.member` im = do
                    return im
                | otherwise = do
                    c <- getBoxedClosureData b
                    --ims <- liftM IntMap.unions (sequence (map (\x -> go x im) (allPtrs c)))
                    ims <- foldM (\im x -> go x im) im (allPtrs c)
                    return $ (ins b c ims)

walkHeapDepth a d = go (asBox a) d IntMap.empty
  where go b d im | d == 0 = return im
                  | (boxAddr b) `IntMap.member` im = do
                      return im
                  | otherwise = do
                      c <- getBoxedClosureData b
                      --ims <- liftM IntMap.unions (sequence (map (\x -> go x im) (allPtrs c)))
                      ims <- foldM (\im x -> go x (d - 1) im) im (allPtrs c)
                      return $ (ins b c ims)

--nameGraph :: IntMap Closure -> [(String, [String])]
--nameGraph im = 
