{-# LANGUAGE MagicHash, DeriveDataTypeable, NoMonomorphismRestriction, RankNTypes #-}

{- |
   Module      : GHC.Vis.Internal
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Internal (
  walkHeap,
  parseBoxes,
  parseBoxesHeap,
  pointersToFollow2,
  showClosure
  )
  where

import Prelude hiding (catch)

import GHC.Vis.Types
import GHC.HeapView hiding (name, pkg, modl, fun, arrWords)

import Control.Monad
import Control.Monad.State hiding (State, fix)

import Data.Word
import Data.Char
import Data.List hiding (insert)

import Text.Printf
import Unsafe.Coerce

import Control.Monad.Trans.Maybe

import System.IO.Unsafe

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names.
parseBoxes :: [(Box, String)] -> IO [[VisObject]]
parseBoxes bs = do
  r <- generalParseBoxes evalState bs
  case r of
    Just x -> return x
    _ -> parseBoxes bs

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names. Also return the resulting 'HeapMap' and another
--   'HeapMap' that does not contain BCO pointers.
parseBoxesHeap :: [(Box, String)] -> IO ([[VisObject]], PState)
parseBoxesHeap bs = do
  r <- generalParseBoxes runState bs
  case r of
    (Just x, y) -> return (x,y)
    _ -> parseBoxesHeap bs

generalParseBoxes ::
     (PrintState (Maybe [[VisObject]]) -> PState -> b)
  -> [(Box, String)] -> IO b
generalParseBoxes f bs = do
  h <- walkHeapSimply bs
  h2 <- walkHeapWithBCO bs
  return $ f (g bs) $ PState 0 0 0 h h2
  where g bs' = runMaybeT $ go bs'
        go ((b',_):b's) = do h <- lift $ gets heapMap
                             (_,c) <- lookupT b' h
                             r <- parseClosure b' c
                             rs <- go b's
                             return $ simplify r : rs
        go [] = return []

lookupT :: Eq a => a -> [(a,b)] -> MaybeT PrintState b
lookupT b' h = MaybeT $ return $ lookup b' h

-- Pulls together multiple Unnamed objects to one
simplify :: [VisObject] -> [VisObject]
simplify [] = []
simplify [Named a bs] = [Named a $ simplify bs]
simplify [a] = [a]
simplify (Unnamed a : Unnamed b : xs) = simplify $ Unnamed (a ++ b) : xs
simplify (Named a bs : xs) = Named a (simplify bs) : simplify xs
simplify (a:xs) = a : simplify xs

parseClosure :: Box -> Closure -> MaybeT PrintState [VisObject]
parseClosure b c = do
  o <- correctObject b
  case o of
    Link n -> return [Link n] -- Don't build infinite heaps
    _      -> do i <- parseInternal b c
                 return $ insertObjects o i

correctObject :: Box -> MaybeT PrintState VisObject
correctObject box = do
  r <- lift $ countReferences box
  n <- getName box

  case n of
    Just name -> return $ Link name
    Nothing -> if r > 1 then
                 (do setName box
                     mbName <- getName box
                     name <- MaybeT $ return mbName
                     return $ Named name [])
                 else return $ Unnamed ""

insertObjects :: VisObject -> [VisObject] -> [VisObject]
insertObjects _ xs@(Function _ : _) = xs
insertObjects _ xs@(Thunk _ : _) = xs
insertObjects (Link name) _ = [Link name]
insertObjects (Named name _) xs = [Named name xs]
insertObjects (Unnamed _) xs = xs
insertObjects _ _ = error "unexpected arguments"

-- | Recursively walk down the heap objects and return the resulting map. This
--   function recognizes loops and avoids them. Big data structures might still
--   be very slow.
walkHeap :: [(Box, String)] -> IO HeapMap
walkHeap = walkHeapGeneral Just pointersToFollow

-- walkHeap, but without Top level names
walkHeapSimply :: [(Box, String)] -> IO HeapMap
walkHeapSimply = walkHeapGeneral (const Nothing) pointersToFollow

walkHeapWithBCO :: [(Box, String)] -> IO HeapMap
walkHeapWithBCO = walkHeapGeneral (const Nothing) pointersToFollow2

walkHeapGeneral :: (String -> Maybe String) -> (Closure -> IO [Box]) -> [(Box, String)] -> IO HeapMap
walkHeapGeneral topF p2fF bs = foldM (topNodes topF) [dummy] bs >>= \s -> foldM (startWalk p2fF) s bs
  where topNodes hn l (b,n) = do -- Adds the top nodes without looking at their pointers
          c' <- getBoxedClosureData b
          return $ insert (b, (hn n, c')) l
        startWalk p2f l (b,_) = do -- Ignores that the top nodes are already in the heap map
          c' <- getBoxedClosureData b
          p  <- p2f c'
          foldM (step p2f) l p
        step p2f l b = case lookup b l of
          Just _  -> return l
          Nothing -> do
            c' <- getBoxedClosureData b
            p  <- p2f c'
            foldM (step p2f) (insert (b, (Nothing, c')) l) p
        dummy = (asBox (1 :: Integer),
          (Nothing, ConsClosure (StgInfoTable 0 0 CONSTR_0_1 0) (map fst bs) [] "" "" ""))

-- Don't inspect deep pointers in BCOClosures for now, they never end

-- New: We're not inspecting the BCOs and instead later looks which of its
-- recursive children are still in the heap. Only those should be visualized.
pointersToFollow :: Closure -> IO [Box]
pointersToFollow (BCOClosure _ _ _ _ _ _ _) = return []

pointersToFollow (MutArrClosure _ _ _ bPtrs) =
  do cPtrs <- mapM getBoxedClosureData bPtrs
     return $ fix $ zip bPtrs cPtrs
  where fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):_:_:xs) = fix xs
        fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):_:xs) = fix xs
        fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):xs) = fix xs
        fix ((x,_):xs) = x : fix xs
        fix [] = []

pointersToFollow x = return $ allPtrs x

-- | Follows 'GHC.HeapView.BCOClosure's, but not the debugging data structures
--   (ByteCodeInstr.BreakInfo) of GHC.
pointersToFollow2 :: Closure -> IO [Box]
pointersToFollow2 (MutArrClosure _ _ _ bPtrs) =
  do cPtrs <- mapM getBoxedClosureData bPtrs
     return $ fix $ zip bPtrs cPtrs
  where fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):_:_:xs) = fix xs
        fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):_:xs) = fix xs
        fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):xs) = fix xs
        fix ((x,_):xs) = x : fix xs
        fix [] = []

pointersToFollow2 x = return $ allPtrs x

-- walkHeap, but with a maximum depth
--walkHeapDepth :: [(Box, String)] -> IO HeapMap
--walkHeapDepth bs = foldM topNodes [dummy bs] bs >>= \s -> foldM goStart s bs
--  where topNodes l (b,n) = do -- Adds the top nodes without looking at their pointers
--            c' <- getBoxedClosureData b
--            return $ insert (b, (Just n, c')) l
--        goStart l (b,_) = do -- Ignores that the top nodes are already in the heap map
--          c' <- getBoxedClosureData b
--          p  <- pointersToFollow c'
--          foldM (\l b -> go l b 30) l p
--        go l _ 0 = return l
--        go l b x = case lookup b l of
--          Just _  -> return l
--          Nothing -> do
--            c' <- getBoxedClosureData b
--            p  <- pointersToFollow c'
--            foldM (\l b -> go l b (x-1)) (insert (b, (Nothing, c')) l) p

-- Additional map operations
insert :: (Box, HeapEntry) -> HeapMap -> HeapMap
insert (b,x) xs = case find (\(c,_) -> c == b) xs of
  Just _  -> xs
  Nothing -> (b,x):xs

adjust :: (HeapEntry -> HeapEntry) -> Box -> HeapMap -> Maybe HeapMap
adjust f b h = do i <- findIndex (\(y,_) -> y == b) h
                  let (h1,(_,x):h2) = splitAt i h
                  return $ h1 ++ ((b,f x) : h2)

setName :: Box -> MaybeT PrintState ()
setName b = do PState ti fi bi h h2 <- lift get
               (_,c) <- lookupT b h
               let (n, ti',fi',bi') = case c of
                     ThunkClosure{} -> ('t' : show ti, ti+1, fi, bi)
                     APClosure{}    -> ('t' : show ti, ti+1, fi, bi)
                     FunClosure{}   -> ('f' : show fi, ti, fi+1, bi)
                     PAPClosure{}   -> ('f' : show fi, ti, fi+1, bi)
                     _              -> ('b' : show bi, ti, fi, bi+1)
                   set (Nothing, closure) = (Just n, closure)
                   set _ = error "unexpected pattern"
               h' <- MaybeT $ return $ adjust set b h
               lift $ put $ PState ti' fi' bi' h' h2

getName :: Box -> MaybeT PrintState (Maybe String)
getName b = do h <- lift $ gets heapMap
               (name,_) <- lookupT b h
               return name

getSetName :: Box -> MaybeT PrintState String
getSetName b = do mn <- getName b
                  case mn of
                    Nothing   -> do setName b
                                    name <- getName b
                                    MaybeT $ return name
                    Just name -> return name

-- How often is a box referenced in the entire heap map
countReferences :: Box -> PrintState Int
countReferences b = do
  h <- gets heapMap'
  return $ sum $ map countR h
 where countR (_,(_,c)) = length $ filter (== b) $ allPtrs c

parseInternal :: Box -> Closure -> MaybeT PrintState [VisObject]
parseInternal _ (ConsClosure _ [] [dataArg] _pkg modl name) =
 return [Unnamed $ case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> name ++ " " ++ show dataArg

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> name ++ " " ++ (show $ (fromIntegral :: Word -> Int) dataArg)

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "D# %0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "F# %0.5f" (unsafeCoerce dataArg :: Double)

    (_,name') -> printf "%s %d" name' dataArg
  ]

-- Empty ByteStrings point to a nullForeignPtr, evaluating it leads to an
-- Prelude.undefined exception
parseInternal _ (ConsClosure (StgInfoTable 1 3 _ _) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return [Unnamed "ByteString 0 0"]

parseInternal _ (ConsClosure (StgInfoTable 1 3 _ _) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- liftM mbParens $ contParse bPtr
       return $ Unnamed (printf "ByteString %d %d " start end) : cPtr

parseInternal _ (ConsClosure (StgInfoTable 2 3 _ _) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- liftM mbParens $ contParse bPtr1
       cPtr2 <- liftM mbParens $ contParse bPtr2
       return $ Unnamed (printf "Chunk %d %d " start end) : cPtr1 ++ [Unnamed " "] ++ cPtr2

parseInternal _ (ConsClosure (StgInfoTable 2 0 _ _) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- liftM mbParens $ contParse bHead
       cTail <- liftM mbParens $ contParse bTail
       return $ cHead ++ [Unnamed ":"] ++ cTail

parseInternal _ (ConsClosure _ bPtrs dArgs _ _ name)
  = do cPtrs <- mapM (liftM mbParens . contParse) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       let sPtrs = if null tPtrs then [Unnamed ""] else Unnamed " " : tPtrs
       return $ Unnamed (intercalate " " $ name : map show dArgs) : sPtrs

parseInternal _ (ArrWordsClosure _ _ arrWords)
  = return $ intercalate [Unnamed ","] (map (\x -> [Unnamed (printf "0x%x" x)]) arrWords)

parseInternal _ (IndClosure _ b)
  = contParse b

parseInternal _ (SelectorClosure _ b)
  = contParse b

parseInternal _ (BlackholeClosure _ b)
  = contParse b

parseInternal _ BlockingQueueClosure{}
  = return [Unnamed "BlockingQueue"]

parseInternal _ (OtherClosure (StgInfoTable _ _ cTipe _) _ _)
  = return [Unnamed $ show cTipe]

parseInternal _ (UnsupportedClosure (StgInfoTable _ _ cTipe _))
  = return [Unnamed $ show cTipe]

-- Reversed order of ptrs
parseInternal b (ThunkClosure _ bPtrs args) = do
  name <- getSetName b
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
      sArgs = Unnamed $ if null args then "" else show args
  return $ Thunk name : sPtrs ++ [sArgs]

parseInternal b (FunClosure _ bPtrs args) = do
  name <- getSetName b
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
      sArgs = Unnamed $ if null args then "" else show args
  return $ Function name : sPtrs ++ [sArgs]

-- bPtrs here can currently point to Nothing, because else we might get infinite heaps
parseInternal _ (MutArrClosure _ _ _ bPtrs)
  = do cPtrs <- mutArrContParse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]

parseInternal _ (MutVarClosure _ b)
  = do c <- contParse b
       return $ Unnamed "MutVar " : c

parseInternal _ (BCOClosure _ _ _ bPtr _ _ _)
  = do cPtrs <- bcoContParse [bPtr]
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
  -- = do case lookup b h of
  --        Nothing -> c <- getBoxedClosureData bPtr
  --        Just (_,c) -> p  <- parseClosure bPtr c
  -- = do vs <- contParse bPtr
  --      let ls = filter isExternal $ filter isLink vs

  --          isLink (Link _) = True
  --          isLink _ = False

  --          isExternal (Link n) = all (notHasName n) vs

  --          notHasName n (Named m _) = n /= m
  --          notHasName n (Function m) = n /= m
  --          notHasName _ _ = True
  --      return vs

parseInternal b (APClosure _ _ _ fun pl) = do
  name <- getSetName b
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Thunk name : fPtr ++ sPtrs

parseInternal b (PAPClosure _ _ _ fun pl) = do
  name <- getSetName b
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Function name : Unnamed "(" : fPtr ++ [Unnamed ")"] ++ sPtrs

parseInternal _ (MVarClosure _ qHead qTail qValue)
   = do cHead <- liftM mbParens $ contParse qHead
        cTail <- liftM mbParens $ contParse qTail
        cValue <- liftM mbParens $ contParse qValue
        return $ Unnamed "MVar#(" : cHead ++ [Unnamed ","] ++ cTail ++ [Unnamed ","] ++ cValue ++ [Unnamed ")"]

contParse :: Box -> MaybeT PrintState [VisObject]
contParse b = do h <- lift $ gets heapMap
                 (_,c) <- lookupT b h
                 parseClosure b c

bcoContParse :: [Box] -> MaybeT PrintState [[VisObject]]
bcoContParse [] = return []
bcoContParse (b:bs) = gets heapMap >>= \h -> case lookup b h of
  Nothing    -> do let ptf = unsafePerformIO $ getBoxedClosureData b >>= pointersToFollow2
                   bcoContParse $ ptf ++ bs -- Could go into infinite loop
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- bcoContParse bs
                   return $ p : ps

mutArrContParse :: [Box] -> MaybeT PrintState [[VisObject]]
mutArrContParse [] = return []
mutArrContParse (b:bs) = gets heapMap >>= \h -> case lookup b h of
  Nothing -> mutArrContParse bs
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- mutArrContParse bs
                   return $ p : ps

-- TODO: Doesn't work quite right, for example with (1,"fo")
mbParens :: [VisObject] -> [VisObject]
mbParens t = if needsParens
    then Unnamed "(" : t ++ [Unnamed ")"]
    else t
  where needsParens = go (0 :: Int) $ show t
        go 0 (' ':_) = True
        go _ [] = False
        go c (')':ts) = go (c-1) ts
        go c ('(':ts) = go (c+1) ts
        go c ('\'':'(':ts) = go c ts
        go c ('\'':')':ts) = go c ts
        go c (_:ts) = go c ts
--mbParens t | ' ' `objElem` t = Unnamed "(" : t ++ [Unnamed ")"]
--           | otherwise     = t
--  where objElem c = any go
--          where go (Unnamed xs) = c `elem` xs
--                go (Named _ os) = any go os
--                go _ = False

-- | Textual representation of Heap objects, used in the graph visualization.
showClosure :: Closure -> String
showClosure (ConsClosure _ _ [dataArg] _ modl name) =
 case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> name ++ " " ++ show dataArg

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> name ++ " " ++ (show $ (fromIntegral :: Word -> Int) dataArg)

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg

    ("Types", "D#") -> printf "D# %0.5f" (unsafeCoerce dataArg :: Double)
    ("Types", "F#") -> printf "F# %0.5f" (unsafeCoerce dataArg :: Double)

    -- :m +GHC.Arr
    -- let b = array ((1,1),(3,2)) [((1,1),42),((1,2),23),((2,1),999),((2,2),1000),((3,1),1001),((3,2),1002)]
    -- b
    -- :view b
    (_,name') -> printf "%s %d" name' dataArg

showClosure (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = "ByteString 0 0"

showClosure (ConsClosure (StgInfoTable 1 3 _ 0) [_] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = printf "ByteString %d %d" start end

showClosure (ConsClosure (StgInfoTable 2 3 _ 1) [_,_] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = printf "Chunk %d %d" start end

showClosure (ConsClosure _ _ dArgs _ _ name)
  = intercalate " " $ name : map show dArgs

-- Reversed order of ptrs
showClosure ThunkClosure{}
  = "Thunk"

showClosure SelectorClosure{}
  = "Selector"

-- Probably should delete these from Graph
showClosure IndClosure{}
  = "Ind"

showClosure BlackholeClosure{}
  = "Blackhole"

showClosure APClosure{}
  = "AP"

showClosure PAPClosure{}
  = "PAP"

showClosure BCOClosure{}
  = "BCO"

showClosure (ArrWordsClosure _ _ arrWords)
  = intercalate ",\n" $ map (printf "0x%x") arrWords

showClosure MutArrClosure{}
  = "MutArr"

showClosure MutVarClosure{}
  = "MutVar"

showClosure MVarClosure{}
  = "MVar#"

showClosure FunClosure{}
  = "Fun"

showClosure BlockingQueueClosure{}
  = "BlockingQueue"

showClosure (OtherClosure (StgInfoTable _ _ cTipe _) _ _)
  = show cTipe

showClosure (UnsupportedClosure (StgInfoTable _ _ cTipe _))
  = show cTipe

--showClosure c = "Missing pattern for " ++ show c
