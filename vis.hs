{-# LANGUAGE MagicHash, BangPatterns #-}

-- BCOClosure says 4 ptrs, has only 3

import GHC.HeapView
import Control.Monad
import Data.Tree
import Data.Word
import Data.Char
import Data.List
import Data.Maybe
import GHC.Prim
import GHC.Exts
import Text.Printf
import Unsafe.Coerce

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap

-- Temporarily, probably use System.Mem.StableName
instance Ord Box
  where compare x y = compare (show x) (show y)

--instance Ord NBox
--  where compare (NamedBox _ a) (NamedBox _ b) = compare a b
--        compare (NamedBox _ a) (UnnamedBox b) = compare a b
--        compare (UnnamedBox a) (UnnamedBox b) = compare a b
--        compare (UnnamedBox a) (NamedBox _ b) = compare a b

data NBox = NamedBox String Box
          | UnnamedBox Box
          deriving (Eq,Ord)

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
              return (tprint h (myLookup (asBox a) h))

myLookup b h = fromJust $ lookup b h

tprint h c@(ConsClosure (StgInfoTable _ _ CONSTR_0_1 _) _ [dataArg] _ modl name) =
 case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> show dataArg

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> show $ (fromIntegral :: Word -> Int) dataArg

    ("Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)

    c -> "Missing ConsClosure pattern for " ++ show c

tprint h (ConsClosure (StgInfoTable 1 3 CONSTR 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = (printf "ByteString[%d,%d]" start end) ++ "(" ++ (tprint h cPtr) ++ ")"
  where cPtr = myLookup bPtr h

tprint h (ConsClosure (StgInfoTable 2 3 CONSTR 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = (printf "Chunk[%d,%d]" start end) ++ "(" ++ (tprint h cPtr1) ++ " | " ++ (tprint h cPtr2) ++ ")"
  where cPtr1 = myLookup bPtr1 h
        cPtr2 = myLookup bPtr2 h

tprint h (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 2) [bPtr] [] _ "GHC.ForeignPtr" "PlainPtr")
  = tprint h cPtr
  where cPtr = myLookup bPtr h

tprint h (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 1) [bPtr] [] _ "Data.Maybe" "Just")
  = "Just " ++ tprint h cPtr
  where cPtr = myLookup bPtr h

tprint h (ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC 0) [] [] _ _ name)
  = name

tprint h (ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC 0) [] args _ _ name)
  = name ++ show args

tprint h (ConsClosure (StgInfoTable 2 0 CONSTR_2_0 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = tprint h cHead ++ ":" ++ tprint h cTail
  where cHead = myLookup bHead h
        cTail = myLookup bTail h

tprint h (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = intercalate "," (map (printf "0x%x") arrWords :: [String])

tprint h (IndClosure (StgInfoTable 1 0 BLACKHOLE 0) b)
  = tprint h c
  where c = myLookup b h

tprint h (ThunkClosure (StgInfoTable 0 0 THUNK_STATIC 3) [] [])
  = "Thunk"

tprint h (ThunkClosure (StgInfoTable 1 1 THUNK_1_1 0) [bPtr] [arg])
  = "Thunk(" ++ (tprint h cPtr) ++ ", " ++ show arg ++ ")"
  where cPtr = myLookup bPtr h

tprint h (ThunkClosure (StgInfoTable 2 0 THUNK_2_0 0) [bPtr1, bPtr2] [])
  = "Thunk(" ++ (tprint h cPtr1) ++ ", " ++ (tprint h cPtr2) ++ ")"
  where cPtr1 = myLookup bPtr1 h
        cPtr2 = myLookup bPtr2 h

tprint h (ThunkClosure (StgInfoTable 3 0 THUNK 0) [bPtr1, bPtr2, bPtr3] [])
  = "Thunk(" ++ (tprint h cPtr1) ++ ", " ++ (tprint h cPtr2) ++ ", " ++ (tprint h cPtr3) ++ ")"
  where cPtr1 = myLookup bPtr1 h
        cPtr2 = myLookup bPtr2 h
        cPtr3 = myLookup bPtr3 h

tprint h (FunClosure (StgInfoTable 0 1 tipe 0) [] [arg])
  = "Fun(" ++ show arg ++ ")"

tprint h (FunClosure (StgInfoTable 2 0 tipe 0) [bPtr1, bPtr2] [])
  = "Fun(" ++ (tprint h cPtr1) ++ ", " ++ (tprint h cPtr2) ++ ")"
  where cPtr1 = myLookup bPtr1 h
        cPtr2 = myLookup bPtr2 h

tprint _ c = "Missing pattern for " ++ show c

tseq (Box a) = seq a ()
