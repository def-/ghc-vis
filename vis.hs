{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.HeapView
import Control.Monad
import Data.Tree
import Data.Word
import GHC.Prim
import GHC.Exts
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

ins :: Box -> Closure -> IntMap.IntMap Closure -> IntMap.IntMap Closure
ins box closure im = IntMap.insert (boxAddr box) closure im

walkHeap a = go (asBox a)
  where go b = do
          c <- getBoxedClosureData b
          ims <- liftM IntMap.unions (sequence (map go (allPtrs c)))
          return $ (ins b c ims)
