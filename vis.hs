{-# LANGUAGE MagicHash, BangPatterns #-}

-- BCOClosure says 4 ptrs, has only 3

import GHC.HeapView
import Control.Monad
import Data.Tree
import Data.Word
import Data.Char
import GHC.Prim
import GHC.Exts
import Text.Printf
import Unsafe.Coerce

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap

buildTree x = do
  cd <- getBoxedClosureData x
  return (cd, allPtrs cd)

t x = unfoldTreeM buildTree x

u = t . asBox

-- infinite tree duh -- needs MVAR_CLEAN at some weird depth (depends on run)
f = let y = id (:) () y
    in getClosureData y

g = let y = id (:) () y
    in y

h x = id (:) x (h x)

isM (MVarClosure _ _ _ _) = True
isM _ = False

isF (FunClosure _ _ _) = True
isF _ = False

--boxAddr :: Box -> Int
--boxAddr (Box any) = fromIntegral $ W# (aToWord# any)

ins :: [(Box, Closure)] -> Box -> Closure -> [(Box, Closure)]
--ins list box closure = (box, closure) : list
ins list box closure = list ++ [(box, closure)]

-- Run performGC from System.Mem first to get pointers not to change as much
-- while executing
walkHeap a = go (asBox a) []
  where go b l = case lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) l (allPtrs c')
                     return $ (ins l' b c')

--walkHeapDepth a d = go (asBox a) d IntMap.empty
--  where go b d im | d == 0 = return im
--                  | (boxAddr b) `IntMap.member` im = do
--                      return im
--                  | otherwise = do
--                      c <- getBoxedClosureData b
--                      --ims <- liftM IntMap.unions (sequence (map (\x -> go x im) (allPtrs c)))
--                      ims <- foldM (\im x -> go x (d - 1) im) im (allPtrs c)
--                      return $ (ins b c ims)

dprint a = do h <- walkHeap a
              case lookup (asBox a) h of
                Nothing -> fail "value not found"
                Just c  -> return $ tprint h c

tprint h c@(ConsClosure (StgInfoTable _ _ CONSTR_0_1 _) _ [dataArg] descr) =
 case descr of
    k | k `elem` [ "base:GHC.Word.W#"
                 ] -> show $ (fromIntegral :: Word -> Int) dataArg

    k | k `elem` [ "integer-gmp:GHC.Integer.Type.S#"
                 , "ghc-prim:GHC.Types.I#"
                 , "ghc-prim:GHC.Types.I8#" -- TODO: Check those
                 , "ghc-prim:GHC.Types.I16#"
                 , "ghc-prim:GHC.Types.I32#"
                 , "ghc-prim:GHC.Types.I64#"
                 ] -> show $ (fromIntegral :: Word -> Int) dataArg

    "ghc-prim:GHC.Types.C#" -> show . chr $ fromIntegral dataArg

    "ghc-prim:GHC.Types.D#" -> printf "%0.5f" (unsafeCoerce dataArg :: Double)
    "ghc-prim:GHC.Types.F#" -> printf "%0.5f" (unsafeCoerce dataArg :: Double)

    c -> "Missing cons pattern for " ++ show c

tprint h c@(ConsClosure (StgInfoTable _ _ CONSTR_NOCAF_STATIC _) _ _ "ghc-prim:GHC.Types.[]")
  = "[]"

tprint h c@(ConsClosure (StgInfoTable _ _ CONSTR_2_0 _) [bHead,bTail] _ "ghc-prim:GHC.Types.:")
  = tprint h cHead ++ ":" ++ tprint h cTail
  where Just cHead = lookup bHead h
        Just cTail = lookup bTail h

tprint h c@(IndClosure (StgInfoTable _ _ BLACKHOLE _) b)
  = tprint h c
  where Just c = lookup b h


tprint _ c = "Missing pattern for " ++ show c

