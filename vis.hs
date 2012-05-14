{-# LANGUAGE MagicHash, BangPatterns #-}

-- BCOClosure says 4 ptrs, has only 3

import GHC.HeapView
import Control.Monad
import Control.Monad.State
import Data.Tree
import Data.Word
import Data.Char
import Data.List
import Data.Maybe
import GHC.Prim
import GHC.Exts
import Text.Printf
import Unsafe.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

--import Data.IntMap(IntMap)
--import qualified Data.IntMap as IntMap

type HeapEntry = (Maybe String, Closure)
type HeapMap   = Map Box HeapEntry
--type PrintState = State HeapMap
type PrintState = State (Integer, HeapMap)

-- TODO: Doesn't work!
-- Temporarily, probably use System.Mem.StableName
instance Ord Box
  where compare x y = compare (show x) (show y)

--instance Ord NBox
--  where compare (NamedBox _ a) (NamedBox _ b) = compare a b
--        compare (NamedBox _ a) (UnnamedBox b) = compare a b
--        compare (UnnamedBox a) (UnnamedBox b) = compare a b
--        compare (UnnamedBox a) (NamedBox _ b) = compare a b

--data NBox = NamedBox String Box
--          | UnnamedBox Box
--          deriving (Eq,Ord)

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

-- Run performGC from System.Mem first to get pointers not to change as much
-- while executing
walkHeap :: a -> IO HeapMap
walkHeap a = go (asBox a) Map.empty
  where go b l = case Map.lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) l (allPtrs c')
                     return $ (Map.insert b (Nothing, c') l')

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
              return $ evalState (mprint (myLookup (asBox a) h)) (0,h)

myLookup b h = fromJust $ Map.lookup b h

mlp :: Box -> PrintState String
mlp b = do (_,h) <- get
           mprint $ fromJust $ Map.lookup b h

mprint :: HeapEntry -> PrintState String
mprint (_,ConsClosure (StgInfoTable _ _ CONSTR_0_1 _) _ [dataArg] _ modl name) =
 return $ case (modl, name) of
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

mprint (_,ConsClosure (StgInfoTable 1 3 CONSTR 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- mlp bPtr
       return $ printf "ByteString[%d,%d](%s)" start end cPtr

mprint (_,ConsClosure (StgInfoTable 2 3 CONSTR 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- mlp bPtr1
       cPtr2 <- mlp bPtr2
       return $ printf "Chunk[%d,%d](%s|%s)" start end cPtr1 cPtr2

mprint (_,ConsClosure (StgInfoTable 1 0 CONSTR_1_0 2) [bPtr] [] _ "GHC.ForeignPtr" "PlainPtr")
  = mlp bPtr

mprint (_,ConsClosure (StgInfoTable 1 0 CONSTR_1_0 1) [bPtr] [] _ _ name)
  = do cPtr <- mlp bPtr
       return $ name ++ cPtr

mprint (_,ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC 0) [] [] _ _ name)
  = return name

mprint (_,ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC 0) [] args _ _ name)
  = return $ name ++ show args

mprint (_,ConsClosure (StgInfoTable 2 0 CONSTR_2_0 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- mlp bHead
       cTail <- mlp bTail
       return $ cHead ++ ":" ++ cTail

mprint (_,ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = return $ intercalate "," (map (printf "0x%x") arrWords :: [String])

mprint (_,IndClosure (StgInfoTable 1 0 BLACKHOLE 0) b)
  = mlp b

mprint (_,ThunkClosure (StgInfoTable 0 0 THUNK_STATIC 3) [] [])
  = return "Thunk"

mprint (_,ThunkClosure (StgInfoTable 1 1 THUNK_1_1 0) [bPtr] [arg])
  = do cPtr <- mlp bPtr
       return $ printf "Thunk(%s, %s)" cPtr (show arg)

mprint (_,ThunkClosure (StgInfoTable 2 0 THUNK_2_0 0) [bPtr1, bPtr2] [])
  = do cPtr1 <- mlp bPtr1
       cPtr2 <- mlp bPtr2
       return $ printf "Thunk(%s, %s)" cPtr1 cPtr2

mprint (_,ThunkClosure (StgInfoTable 3 0 THUNK 0) [bPtr1, bPtr2, bPtr3] [])
  = do cPtr1 <- mlp bPtr1
       cPtr2 <- mlp bPtr2
       cPtr3 <- mlp bPtr3
       return $ printf "Thunk(%s, %s, %s)" cPtr1 cPtr2 cPtr3

mprint (_,FunClosure (StgInfoTable 0 1 tipe 0) [] [arg])
  = return $ "Fun(" ++ show arg ++ ")"

mprint (_,FunClosure (StgInfoTable 2 0 tipe 0) [bPtr1, bPtr2] [])
  = do cPtr1 <- mlp bPtr1
       cPtr2 <- mlp bPtr2
       return $ printf "Fun(%s, %s)" cPtr1 cPtr2

mprint c = return $ "Missing pattern for " ++ show c

tseq (Box a) = seq a ()
