{-# LANGUAGE MagicHash, BangPatterns #-}

module GHC.Vis (
  walkHeap,
  walkHeapWithoutDummy,
  parseBoxes,
  parseBoxesHeap,
  parseClosure,
  VisObject(..),
  State(..),
  pointersToFollow,
  HeapMap,
  showClosure
  )
  where

import GHC.HeapView

import Control.DeepSeq
import Control.Monad
import Control.Monad.State hiding (State)
import qualified Control.Monad.State as MS
import Data.Tree
import Data.Graph.Inductive
import Data.Word
import Data.Char
import Data.List hiding (insert)
import Data.Maybe
import GHC.Prim
import GHC.Exts
import Text.Printf
import Unsafe.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

data State = State
  { boxes :: [Box]
  , objects :: [[VisObject]]
  , bounds :: [(String, (Double, Double, Double, Double))]
  , bounds2 :: [(Int, (Double, Double, Double, Double))]
  , mousePos :: (Double, Double)
  , hover :: Maybe String
  , hover2 :: Maybe Int
  }

type HeapEntry = (Maybe String, Closure)
-- We're using a slow, eq-based list instead of a proper map because
-- StableNames' hash values aren't stable enough
type HeapMap   = [(Box, HeapEntry)]
type PrintState = MS.State (Integer, HeapMap)

data VisObject = Unnamed String
               | Named String [VisObject]
               | Link String
               | Function String
               deriving Eq

instance Show VisObject where
  show (Unnamed x) = x
  show (Named x ys) = x ++ "=(" ++ show ys ++ ")"
  show (Link x) = x
  show (Function x) = x

  showList []       = showString ""
  showList (c:cs)   = showString (show c) . showList cs

parseBoxes = generalParseBoxes evalState

parseBoxesHeap = generalParseBoxes runState

generalParseBoxes f bs = walkHeap bs >>= \h -> return $ f (go bs) (0,h)
  where go (b:bs) = do (_,h) <- get
                       r <- parseClosure b (snd $ fromJust $ lookup b h)
                       rs <- go bs
                       --return (r:rs)
                       return ((simplify r):rs)
        go [] = return []

-- Pulls together multiple Unnamed objects to one
simplify :: [VisObject] -> [VisObject]
simplify [] = []
simplify [Named a bs] = [Named a (simplify bs)]
simplify [a] = [a]
simplify ((Unnamed a):(Unnamed b):xs) = simplify $ (Unnamed (a ++ b)):xs
simplify ((Named a bs):xs) = (Named a (simplify bs)) : (simplify xs)
simplify (a:xs) = a : (simplify xs)

parseClosure :: Box -> Closure -> PrintState [VisObject]
parseClosure b c = do
  o <- correctObject b
  case o of
    Link n -> return [Link n] -- Don't build infinite heaps
    otherwise -> do i <- parseInternal b c
                    return $ insertObjects o i

correctObject :: Box -> PrintState VisObject
correctObject box = do
  i <- getCount
  r <- countReferences box
  n <- getName box

  case n of
    Just name -> return $ Link name
    Nothing -> case r > 1 of
                    True -> do setName box
                               name <- liftM fromJust $ getName box
                               return $ Named name []
                    False -> return $ Unnamed ""

insertObjects :: VisObject -> [VisObject] -> [VisObject]
insertObjects _ xs@((Function name):_) = xs
insertObjects (Link name) _ = [Link name]
insertObjects (Named name _) xs = [Named name xs]
insertObjects (Unnamed _) xs = xs

walkHeapWithoutDummy :: [Box] -> IO HeapMap
walkHeapWithoutDummy bs = do
  -- Add a special pointer to detect number of pointers to start boxes
  foldM (\l b -> go b l) [] bs
  where go b l = case lookup b l of
          Just _  -> return l
          Nothing -> do
            c' <- getBoxedClosureData b
            p  <- pointersToFollow c'
            l' <- foldM (\l x -> go x l) (insert (b, (Nothing, c')) l) p
            return $ insert (b, (Nothing, c')) l'

walkHeap :: [Box] -> IO HeapMap
walkHeap bs = do
  -- Add a special pointer to detect number of pointers to start boxes
  foldM (\l b -> go b l) [dummy] bs
  where dummy = (asBox 1,
          (Nothing, ConsClosure (StgInfoTable 0 0 CONSTR_0_1 0) bs [] "" "" ""))
        go b l = case lookup b l of
          Just _  -> return l
          Nothing -> do
            c' <- getBoxedClosureData b
            p  <- pointersToFollow c'
            l' <- foldM (\l x -> go x l) (insert (b, (Nothing, c')) l) p
            return $ insert (b, (Nothing, c')) l'

-- Don't inspect deep pointers in BCOClosures for now, they never end
pointersToFollow (MutArrClosure (StgInfoTable _ _ _ _) _ _ bPtrs) =
  do cPtrs <- mapM getBoxedClosureData bPtrs
     return $ fix $ zip bPtrs cPtrs
  where fix ((_,(ConsClosure (StgInfoTable _ _ _ _) _ _ _ "ByteCodeInstr" "BreakInfo")):_:_:xs) = fix xs
        fix ((_,(ConsClosure (StgInfoTable _ _ _ _) _ _ _ "ByteCodeInstr" "BreakInfo")):_:xs) = fix xs
        fix ((x,_):xs) = x : fix xs
        fix [] = []

pointersToFollow x = return $ allPtrs x

-- Additional map operations
insert :: (Box, HeapEntry) -> HeapMap -> HeapMap
insert (b,x) xs = case find (\(c,y) -> c == b) xs of
  Just _  -> xs
  Nothing -> (b,x):xs

adjust :: (HeapEntry -> HeapEntry) -> Box -> HeapMap -> HeapMap
adjust f b h = h1 ++ ((b,f x) : h2)
  where i = fromJust $ findIndex (\(x,_) -> x == b) h
        (h1,(b1,x):h2) = splitAt i h

setName :: Box -> PrintState ()
setName b = modify go
  where go (i,h) = (i + 1, adjust (set i) b h)
        set i (Nothing, closure) = (Just ("t" ++ show i), closure)

getName :: Box -> PrintState (Maybe String)
getName b = do (_,h) <- get
               return $ fst $ fromJust $ lookup b h

getSetName :: Box -> PrintState String
getSetName b = do mn <- getName b
                  case mn of
                    Nothing   -> do setName b
                                    n <- getName b
                                    return $ fromJust n
                    Just name -> return name

getCount :: PrintState Integer
getCount = do (i,_) <- get
              return i

-- How often is a box referenced in the entire heap map
countReferences :: Box -> PrintState Int
countReferences b = do
  (_,h) <- get
  return $ sum $ map countR h
 where countR (_,(_,c)) = length $ filter (== b) $ allPtrs c

parseInternal _ (ConsClosure (StgInfoTable _ _ _ _) _ [dataArg] _ modl name) =
 return [Unnamed $ case (modl, name) of
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

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)

    c -> "Missing ConsClosure pattern for " ++ show c
  ]

-- Empty ByteStrings point to a nullForeignPtr, evaluating it leads to an
-- Prelude.undefined exception
parseInternal _ (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return [Unnamed "ByteString[0,0]()"]

parseInternal _ (ConsClosure (StgInfoTable 1 3 _ 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- contParse bPtr
       return $ (Unnamed (printf "ByteString[%d,%d](" start end)) : cPtr ++ [Unnamed ")"]

parseInternal _ (ConsClosure (StgInfoTable 2 3 _ 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- contParse bPtr1
       cPtr2 <- contParse bPtr2
       return $ (Unnamed (printf "Chunk[%d,%d](" start end)) : cPtr1 ++ [Unnamed ","] ++ cPtr2 ++ [Unnamed ")"]

parseInternal _ (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 _) [bPtr] [] _ _ name)
  = do cPtr <- contParse bPtr
       return $ (Unnamed (name ++ " ")) : mbParens cPtr

parseInternal _ (ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC _) [] [] _ _ name)
  = return [Unnamed name]

parseInternal _ (ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC _) [] args _ _ name)
  = return [Unnamed (name ++ " " ++ show args)]

parseInternal _ (ConsClosure (StgInfoTable 2 0 _ 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- liftM mbParens $ contParse bHead
       cTail <- liftM mbParens $ contParse bTail
       return $ cHead ++ [Unnamed ":"] ++ cTail

parseInternal _ (ConsClosure (StgInfoTable _ 0 _ _) bPtrs [] _ _ name)
  = do cPtrs <- mapM ((liftM mbParens) . contParse) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       return $ (Unnamed (name ++ " ")) : tPtrs

parseInternal _ (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = return $ intercalate [Unnamed ","] (map (\x -> [Unnamed (printf "0x%x" x)]) arrWords)

parseInternal _ (IndClosure (StgInfoTable 1 0 _ 0) b)
  = contParse b

parseInternal _ (BlackholeClosure (StgInfoTable 1 0 _ 0) b)
  = contParse b

-- Reversed order of ptrs
parseInternal b (ThunkClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM contParse $ reverse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ case null args of
         True  -> (Function name) : Unnamed "(" : tPtrs ++ [Unnamed ")"]
         False -> (Function name) : (Unnamed $ show args ++ "(") : tPtrs ++ [Unnamed ")"]

parseInternal b (FunClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM contParse $ reverse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ case null args of
         True  -> (Function name) : Unnamed "(" : tPtrs ++ [Unnamed ")"]
         False -> (Function name) : (Unnamed $ show args ++ "(") : tPtrs ++ [Unnamed ")"]

-- bPtrs here can currently point to Nothing, because else we might get infinite heaps
parseInternal _ (MutArrClosure (StgInfoTable _ _ _ _) _ _ bPtrs)
  = do cPtrs <- mutArrContParse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ Unnamed "(" : tPtrs ++ [Unnamed ")"]

parseInternal _ (BCOClosure (StgInfoTable 4 0 BCO 0) _ _ bPtr _ _ _)
  = contParse bPtr

parseInternal b (APClosure (StgInfoTable 0 0 _ _) _ _ fun _)
  = do cPtr <- contParse fun
       getSetName b >>= \x -> return $ [Function x] ++ cPtr

parseInternal b (PAPClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = getSetName b >>= \x -> return [Function x]

parseInternal _ c = return [Unnamed ("Missing pattern for " ++ show c)]

contParse b@(Box a) = get >>= \(_,h) -> parseClosure b (snd $ fromJust $ lookup b h)

mutArrContParse [] = return []
mutArrContParse ((b@(Box a)):bs) = get >>= \(_,h) -> case lookup b h of
  Nothing -> mutArrContParse bs
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- mutArrContParse bs
                   return $ p : ps

-- TODO: Doesn't work quite right, for example with (1,"fo")
mbParens t@((Unnamed ('"':_)):_) = t
mbParens t@((Unnamed ('(':_)):_) = t
mbParens t | ' ' `objElem` t = Unnamed "(" : t ++ [Unnamed ")"]
            | otherwise     = t

objElem c t = any go t
  where go (Unnamed xs) = c `elem` xs
        go (Named _ os) = any go os
        go _ = False

showClosure (ConsClosure (StgInfoTable _ _ _ _) _ [dataArg] _ modl name) =
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

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)

    c -> "Missing ConsClosure pattern for " ++ show c

showClosure (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = "ByteString[0,0]()"

showClosure (ConsClosure (StgInfoTable 1 3 _ 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = printf "ByteString[%d,%d](" start end

showClosure (ConsClosure (StgInfoTable 2 3 _ 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = printf "Chunk[%d,%d](" start end

showClosure (ConsClosure (StgInfoTable _ _ _ _) _ _ _ _ name)
  = name

showClosure (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = intercalate ", " $ map (\x -> printf "0x%x" x) arrWords

-- Probably should delete these from Graph
showClosure (IndClosure (StgInfoTable 1 0 _ 0) b)
  = "Ind"

showClosure (BlackholeClosure (StgInfoTable 1 0 _ 0) b)
  = "Blackhole"

-- Reversed order of ptrs
showClosure (ThunkClosure (StgInfoTable _ _ _ _) bPtrs args)
  = "Thunk"

showClosure (FunClosure (StgInfoTable _ _ _ _) bPtrs args)
  = "Fun"

showClosure (MutArrClosure (StgInfoTable _ _ _ _) _ _ bPtrs)
  = "MutArr"

showClosure (BCOClosure (StgInfoTable 4 0 BCO 0) _ _ bPtr _ _ _)
  = "BCO"

showClosure (APClosure (StgInfoTable 0 0 _ _) _ _ fun _)
  = "AP"

showClosure (PAPClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = "PAP"

showClosure c = "Missing pattern for " ++ show c

