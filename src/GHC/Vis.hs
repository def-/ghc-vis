{-# LANGUAGE MagicHash, BangPatterns #-}

module GHC.Vis (
  walkHeap,
  mWalkHeap,
  dprint,
  bprint,
  mprint,
  fparse,
  fmmp,
  fmparse,
  fhmparse,
  parseClosure,
  VisObject(..),
  getID,
  buildGraph,
  eval,
  evalS,
  evalP,
  printP,
  )
  where

import GHC.HeapView
import System.Mem
import System.Mem.StableName
import System.IO.Unsafe

import Control.DeepSeq
import Control.Monad
import Control.Monad.State
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

fromJust2 :: String -> Maybe a -> a
fromJust2 _ (Just a) = a
fromJust2 s Nothing = error ("fromJust" ++ s)

--import Data.IntMap(IntMap)
--import qualified Data.IntMap as IntMap

type HeapEntry = (Maybe String, Closure)
type HeapMap   = [(Box, HeapEntry)]
--type PrintState = State HeapMap
type PrintState = State (Integer, HeapMap)

instance Ord Box
  -- TODO: Doesn't work!
  -- Temporarily, probably use System.Mem.StableName
  --where compare x y = compare (show x) (show y)
  -- Ok, this works for now
  where compare (Box x) (Box y) = unsafePerformIO $ (do
          a <- makeStableName x
          b <- makeStableName y
          return $ compare (hashStableName a) (hashStableName b)
          )

data VisObject = Unnamed String
               | Named String [VisObject]
               | Link String
               | Function String

instance Show VisObject where
  show (Unnamed x) = x
  show (Named x ys) = x ++ "=(" ++ show ys ++ ")"
  show (Link x) = x
  show (Function x) = x

  showList []       = showString ""
  showList (c:cs)   = showString (show c) . showList cs

getID a = makeStableName a >>= return . hashStableName

buildGraph :: [Box] -> IO (Gr Closure ())
buildGraph boxes = foldM go empty boxes
  where go graph box@(Box content) = do
          closure  <- getBoxedClosureData box
          nodename <- getID content
          edges <- mapM (\(Box x) -> getID x >>= \target -> return (nodename, target, ())) (nearlyAllPtrs closure)
          if nodename `gelem` graph
            then return graph
            else do
                  graph' <- foldM go (insNode (nodename, closure) graph) $ nearlyAllPtrs closure 
                  return $ insEdges edges graph'

--graphToVisObjectList :: Gr Closure () -> [Box] -> [[VisObject]]
--graphToVisObjectList graph startnodes = mapM go startnodes
--  where go node = if indeg graph node <= 1
--                  then
--                  else NamedObject node (packVisObjects 

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

-- Don't inspect deep pointers in BCOClosures for now, they never end
-- TODO: What we could do is output the instrs and literals of it
nearlyAllPtrs (BCOClosure (StgInfoTable _ _ _ _) i l _b _ _ _) = []
--nearlyAllPtrs x@(BCOClosure _ _ _ _ _ _ _) = filter notBreakInfo $ allPtrs x
--  where notBreakInfo =
nearlyAllPtrs x = allPtrs x

mWalkHeap :: [Box] -> IO HeapMap
mWalkHeap bs = do
  --performGC
  -- Add a special pointer to detect number of pointers to start boxes
  foldM (\l b -> go b l) [dummy] bs
  where dummy = (asBox 1, (Nothing, ConsClosure (StgInfoTable 0 0 CONSTR_0_1 0) bs [] "" "" ""))
        go b l = case lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) (insert (b, (Nothing, c')) l) (nearlyAllPtrs c')
                     return $ insert (b, (Nothing, c')) l'

-- Run performGC from System.Mem first to get pointers not to change as much
-- while executing
-- Not relevant anymore, was only necessary because of StableNames it seems
walkHeap :: a -> IO HeapMap
walkHeap a = do
  --performGC
  go (asBox a) [dummy]
  where dummy = (asBox 1, (Nothing, ConsClosure (StgInfoTable 0 0 CONSTR_0_1 0) [asBox a] [] "" "" ""))
        go b l = case lookup b l of
                   Just _  -> return l
                   Nothing -> do
                     c' <- getBoxedClosureData b
                     l' <- foldM (\l x -> go x l) (insert (b, (Nothing, c')) l) (nearlyAllPtrs c')
                     return $ insert (b, (Nothing, c')) l'

walkHeapDepth a d = do
  --performGC
  go (asBox a) d [dummy]
  where dummy = (asBox 1, (Nothing, ConsClosure (StgInfoTable 0 0 CONSTR_0_1 0) [asBox a] [] "" "" ""))
        go b d l | d == 0 = return l
                 | otherwise = case lookup b l of
                     Just _  -> return l
                     Nothing -> do
                       c' <- getBoxedClosureData b
                       l' <- foldM (\l x -> go x (d - 1) l) (insert (b, (Nothing, c')) l) (nearlyAllPtrs c')
                       return $ insert (b, (Nothing, c')) l'

--walkHeapDepth a d = go (asBox a) d Int[]
--  where go b d im | d == 0 = return im
--                  | (boxAddr b) `IntMap.member` im = do
--                      return im
--                  | otherwise = do
--                      c <- getBoxedClosureData b
--                      --ims <- liftM IntMap.unions (sequence (map (\x -> go x im) (allPtrs c)))
--                      ims <- foldM (\im x -> go x (d - 1) im) im (allPtrs c)
--                      return $ (ins b c ims)

-- TODO: Proper forced evalution
eval a name = do (_,hm) <- dprint a
                 (show $ map go hm) `deepseq` return ()

  where go ((Box a),(Just n, y)) | n == name = seq a (Just n, y)
                                 | otherwise = (Just n, y)
        go (_,(x,y)) = (x,y)


evalS a name = do eval a name
                  bprint a

evalP a name = do eval a name
                  bprint a >>= putStrLn

printP a = bprint a >>= putStrLn

bprint :: a -> IO String
bprint a = do h <- walkHeap a
              return $ evalState (mprint b (snd $ fromJust $ lookup b h)) (0,h)
  where b = asBox a

dprint :: a -> IO (String, HeapMap)
dprint a = do h <- walkHeap a
              return $ evalState (do
                s <- mprint b (snd $ fromJust $ lookup b h)
                (_,h) <- get
                return (s,h)
                ) (0,h)
  where b = asBox a

-- This fromJust can fail because of garbage collection
-- That means some pointer to b inside the data structure has changed
--
-- TODO: Should we copy the whole structure beforehand?
--       Or catch the error and reread the heap?
--       Nope, this probably means my "instance Ord Box" is wrong,
--       makeStableName doesn't seem so stalbe after all
mlp :: Box -> PrintState String
mlp b = do (_,h) <- get
           mprint b (snd $ fromJust $ lookup b h)

insert (b,x) xs = case find (\(c,y) -> c == b) xs of
  Just _  -> xs
  Nothing -> (b,x):xs

adjust f b h = h1 ++ ((b,f x) : h2)
  where i = fromJust $ findIndex (\(x,_) -> x == b) h
        (h1,(b1,x):h2) = splitAt i h

setName :: Box -> PrintState ()
setName b = modify go
  where go (i,h) = (i + 1, adjust (set i) b h)
        set i (Nothing, closure) = (Just ("t" ++ show i), closure)

-- This fromJust can fail because of garbage collection
-- That means some pointer to b inside the data structure has changed
getName :: Box -> PrintState (Maybe String)
getName b = do (_,h) <- get
               return $ fst $ fromJust $ lookup b h


getCount :: PrintState Integer
getCount = do (i,_) <- get
              return i

getSetName :: Box -> PrintState String
getSetName b = do mn <- getName b
                  case mn of
                    Nothing   -> do setName b
                                    n <- getName b
                                    return $ fromJust2 "3" n
                    Just name -> return name

mbParens t@('"':_) = t
mbParens t@('(':_) = t
mbParens t | ' ' `elem` t = "(" ++ t ++ ")"
           | otherwise    = t

countReferences :: Box -> PrintState Int
countReferences b = do
  (_,h) <- get
  return $ sum $ map countR h
 where countR (_,(_,c)) = length $ filter (== b) $ allPtrs c

mprint :: Box -> Closure -> PrintState String
mprint _ (ConsClosure (StgInfoTable _ _ _ _) _ [dataArg] _ modl name) =
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

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "%0.5f" (unsafeCoerce dataArg :: Double)

    c -> "Missing ConsClosure pattern for " ++ show c

-- Empty ByteStrings point to a nullForeignPtr, evaluating it leads to an
-- Prelude.undefined exception
mprint _ (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return "ByteString[0,0]()"

mprint _ (ConsClosure (StgInfoTable 1 3 _ 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- mlp bPtr
       return $ printf "ByteString[%d,%d](%s)" start end cPtr

mprint _ (ConsClosure (StgInfoTable 2 3 _ 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- mlp bPtr1
       cPtr2 <- mlp bPtr2
       return $ printf "Chunk[%d,%d](%s,%s)" start end cPtr1 cPtr2

mprint _ (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 _) [bPtr] [] _ _ name)
  = do cPtr <- mlp bPtr
       return $ name ++ " " ++ mbParens cPtr

mprint _ (ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC _) [] [] _ _ name)
  = return name

mprint _ (ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC _) [] args _ _ name)
  = return $ name ++ " " ++ show args

mprint b (ConsClosure (StgInfoTable 2 0 _ 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = do b' <- getName b
       case b' of
         Just n  -> return n
         Nothing -> do i <- getCount
                       setName b
                       n <- liftM (fromJust2 "4") $ getName b
                       cHead <- liftM mbParens $ mlp bHead
                       cTail <- liftM mbParens $ mlp bTail
                       r <- countReferences b
                       return $ case r > 1 || (i == 0 && r == 1) of
                         True -> printf "%s=(%s:%s)" n cHead cTail
                         False -> printf "%s:%s" cHead cTail

mprint b (ConsClosure (StgInfoTable _ 0 _ _) bPtrs [] _ _ name)
  = do b' <- getName b
       case b' of
         Just n  -> return n
         Nothing -> do i <- getCount
                       setName b
                       n <- liftM (fromJust2 "5") $ getName b
                       cPtrs <- mapM ((liftM mbParens) . mlp) bPtrs
                       let tPtrs = intercalate " " cPtrs
                       r <- countReferences b
                       return $ case r > 1 || (i == 0 && r == 1) of
                         True -> printf "%s=(%s %s)" n name tPtrs
                         False -> printf "%s %s" name tPtrs


mprint _ (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = return $ intercalate "," (map (printf "0x%x") arrWords :: [String])

mprint _ (IndClosure (StgInfoTable 1 0 _ 0) b)
  = mlp b

-- Reversed order of ptrs
mprint b (ThunkClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM mlp $ reverse bPtrs
       let tPtrs = intercalate "," cPtrs
       return $ case null args of
         True  -> name ++ "(" ++ tPtrs ++ ")"
         False -> name ++ show args ++ "(" ++ tPtrs ++ ")"

mprint b (FunClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM mlp $ reverse bPtrs
       let tPtrs = intercalate "," cPtrs
       return $ case null args of
         True  -> name ++ "(" ++ tPtrs ++ ")"
         False -> name ++ show args ++ "(" ++ tPtrs ++ ")"

mprint b (APClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = getSetName b

-- λ> let f x = 3 * x
-- λ> let a = map f [1..3]
-- λ> printP a
-- t0
-- λ> evalP a "t0"
-- t1(1,t2):t3(t4(t5(3,1),1,1),t2)
-- λ> evalP a "t3"
-- t1(1,t2):t4(2,t2):t5(t6(t7(3,1),2,1),t2)
-- λ> evalP a "t5"
-- t1(1,t2):t4(2,t2):t6(3,t2):t7(t8(t9(3,1),3,1),t2)
-- λ> evalP a "t6"
-- (t1(1,Missing pattern for PAPClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = PAP, srtlen = 0}, arity = 2526134728, n_args = 32539, fun = 0x00007f1b9691c1c8, payload = [0x0000000041a6edf0]})):*** Exception: fromJust2

mprint b (PAPClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = getSetName b

-- λ> let f x = 3 * x
-- λ> let a = map f [1..3]
-- λ> printP a
-- t0
-- λ> evalP a "t0"
-- t1(1,t2):t3(t4(t5(3,1),1,1),t2)
-- λ> evalP a "t3"
-- t1(1,t2):t4(2,t2):t5(t6(t7(3,1),2,1),t2)
-- λ> evalP a "t5"
-- t1(1,t2):t4(2,t2):t6(3,t2):t7(t8(t9(3,1),3,1),t2)
-- λ> evalP a "t6"
-- *** Exception: fromJust1

-- λ> let h = let y = id (:) () y in h
-- λ> h
-- ^CInterrupted.
-- λ> walkHeap h
-- *** Exception: getClosureData: Cannot handle closure type AP_STACK


mprint _ c = return $ "Missing pattern for " ++ show c

pullTogether :: [VisObject] -> [VisObject]
pullTogether [] = []
pullTogether [Named a bs] = [Named a (pullTogether bs)]
pullTogether [a] = [a]
pullTogether ((Unnamed a):(Unnamed b):xs) = pullTogether $ (Unnamed (a ++ b)):xs
pullTogether ((Named a bs):xs) = (Named a (pullTogether bs)) : (pullTogether xs)
pullTogether (a:xs) = a : (pullTogether xs)

correctObject :: Box -> PrintState VisObject
correctObject box = do
  i <- getCount
  r <- countReferences box
  n <- getName box

  case n of
    Just name -> return $ Link name
    --Nothing -> case r > 1 || (i == 0 && r == 1) of -- Special case for start nodes
    Nothing -> case r > 1 of
                    True -> do setName box
                               name <- liftM (fromJust2 "4") $ getName box
                               return $ Named name []
                    False -> return $ Unnamed ""

insertObjects :: VisObject -> [VisObject] -> [VisObject]
insertObjects _ xs@((Function name):_) = xs
insertObjects (Link name) _ = [Link name]
insertObjects (Named name _) xs = [Named name xs]
insertObjects (Unnamed _) xs = xs

--fparse :: a -> IO String
fparse a = do h <- walkHeap a
              return $ pullTogether $ evalState (parseClosure b (snd $ fromJust $ lookup b h)) (0,h)
  where b = asBox a

fmmp bs h = return $ evalState (go bs) (0,h)
  where go (b:bs) = do (_,h) <- get
                       r <- parseClosure b (snd $ fromJust $ lookup b h)
                       rs <- go bs
                       return ((pullTogether r):rs)
        go [] = return []

fmparse bs = mWalkHeap bs >>= fmmp bs

fhmmp bs h = return $ runState (go bs) (0,h)
  where go (b:bs) = do (_,h) <- get
                       r <- parseClosure b (snd $ fromJust $ lookup b h)
                       rs <- go bs
                       return ((pullTogether r):rs)
        go [] = return []

fhmparse bs = mWalkHeap bs >>= fhmmp bs

--fmparse bs = do h <- mWalkHeap bs
--                return $ mapM (\b -> pullTogether $ evalState (parseClosure b (snd $ h Map.! b)) (0,h)) bs
--  where b = asBox a

--testp b h = pullTogether $ evalState (parseClosure b (snd $ h Map.! b)) (0,h)

flp b@(Box a) = do (_,h) <- get
                   parseClosure b (snd $ (fromJust2 "1") $ lookup b h)

fmbParens t@((Unnamed ('"':_)):_) = t
fmbParens t@((Unnamed ('(':_)):_) = t
fmbParens t | ' ' `vElem` t = Unnamed "(" : t ++ [Unnamed ")"]
--            | ':' `vElem` t = Unnamed "(" : t ++ [Unnamed ")"]
            | otherwise     = t

vElem c t = any go t
  where go (Unnamed xs) = c `elem` xs
        go (Named _ os) = any go os
        go _ = False

parseClosure :: Box -> Closure -> PrintState [VisObject]
parseClosure b c = do
  o <- correctObject b
  case o of
    Link n -> return [Link n] -- Don't build infinite heaps
    otherwise -> do i <- parseInternal b c
                    return $ insertObjects o i

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
  = do cPtr  <- flp bPtr
       return $ (Unnamed (printf "ByteString[%d,%d](" start end)) : cPtr ++ [Unnamed ")"]

parseInternal _ (ConsClosure (StgInfoTable 2 3 _ 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- flp bPtr1
       cPtr2 <- flp bPtr2
       return $ (Unnamed (printf "Chunk[%d,%d](" start end)) : cPtr1 ++ [Unnamed ","] ++ cPtr2 ++ [Unnamed ")"]

parseInternal _ (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 _) [bPtr] [] _ _ name)
  = do cPtr <- flp bPtr
       return $ (Unnamed (name ++ " ")) : fmbParens cPtr

parseInternal _ (ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC _) [] [] _ _ name)
  = return [Unnamed name]

parseInternal _ (ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC _) [] args _ _ name)
  = return [Unnamed (name ++ " " ++ show args)]

parseInternal _ (ConsClosure (StgInfoTable 2 0 _ 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- liftM fmbParens $ flp bHead
       cTail <- liftM fmbParens $ flp bTail
       return $ cHead ++ [Unnamed ":"] ++ cTail

parseInternal _ (ConsClosure (StgInfoTable _ 0 _ _) bPtrs [] _ _ name)
  = do cPtrs <- mapM ((liftM fmbParens) . flp) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       return $ (Unnamed name) : tPtrs

parseInternal _ (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) bytes arrWords)
  = return $ intercalate [Unnamed ","] (map (\x -> [Unnamed (printf "0x%x" x)]) arrWords)

parseInternal _ (IndClosure (StgInfoTable 1 0 _ 0) b)
  = flp b

-- Reversed order of ptrs
parseInternal b (ThunkClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM flp $ reverse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ case null args of
         True  -> (Function name) : Unnamed "(" : tPtrs ++ [Unnamed ")"]
         False -> (Function name) : (Unnamed $ show args ++ "(") : tPtrs ++ [Unnamed ")"]

parseInternal b (FunClosure (StgInfoTable _ _ _ _) bPtrs args)
  = do name <- getSetName b
       cPtrs <- mapM flp $ reverse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ case null args of
         True  -> (Function name) : Unnamed "(" : tPtrs ++ [Unnamed ")"]
         False -> (Function name) : (Unnamed $ show args ++ "(") : tPtrs ++ [Unnamed ")"]

parseInternal b (APClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = getSetName b >>= \x -> return [Function x]

-- λ> let f x = 3 * x
-- λ> let a = map f [1..3]
-- λ> printP a
-- t0
-- λ> evalP a "t0"
-- t1(1,t2):t3(t4(t5(3,1),1,1),t2)
-- λ> evalP a "t3"
-- t1(1,t2):t4(2,t2):t5(t6(t7(3,1),2,1),t2)
-- λ> evalP a "t5"
-- t1(1,t2):t4(2,t2):t6(3,t2):t7(t8(t9(3,1),3,1),t2)
-- λ> evalP a "t6"
-- (t1(1,Missing pattern for PAPClosure {info = StgInfoTable {ptrs = 0, nptrs = 0, tipe = PAP, srtlen = 0}, arity = 2526134728, n_args = 32539, fun = 0x00007f1b9691c1c8, payload = [0x0000000041a6edf0]})):*** Exception: fromJust2

parseInternal b (PAPClosure (StgInfoTable 0 0 _ _) _ _ _ _)
  = getSetName b >>= \x -> return [Function x]

-- λ> let f x = 3 * x
-- λ> let a = map f [1..3]
-- λ> printP a
-- t0
-- λ> evalP a "t0"
-- t1(1,t2):t3(t4(t5(3,1),1,1),t2)
-- λ> evalP a "t3"
-- t1(1,t2):t4(2,t2):t5(t6(t7(3,1),2,1),t2)
-- λ> evalP a "t5"
-- t1(1,t2):t4(2,t2):t6(3,t2):t7(t8(t9(3,1),3,1),t2)
-- λ> evalP a "t6"
-- *** Exception: fromJust1

-- λ> let h = let y = id (:) () y in h
-- λ> h
-- ^CInterrupted.
-- λ> walkHeap h
-- *** Exception: getClosureData: Cannot handle closure type AP_STACK


parseInternal _ c = return [Unnamed ("Missing pattern for " ++ show c)]

tseq (Box a) = seq a ()

data Foo = Foo Int Int Int Int

a x = 2 * x
