{-# LANGUAGE MagicHash #-}

{- |
   Module      : GHC.Vis.Internal
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Internal (
  walkHeap,
  --walkHeapDepth,
  parseBoxes,
  parseBoxesHeap,
  --parseClosure,
  --pointersToFollow,
  pointersToFollow2,
  showClosure
  )
  where

import GHC.Vis.Types
import GHC.HeapView hiding (name, pkg, modl, fun, arrWords)

import Control.Monad
import Control.Monad.State hiding (State, fix)
import Data.Word
import Data.Char
import Data.List hiding (insert)
import Text.Printf
import Unsafe.Coerce

import System.IO.Unsafe

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names.
parseBoxes :: [(Box, String)] -> IO [[VisObject]]
parseBoxes = generalParseBoxes evalState

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names. Also return the resulting 'HeapMap' and another
--   'HeapMap' that does not contain BCO pointers.
parseBoxesHeap :: [(Box, String)] -> IO ([[VisObject]], (Integer, HeapMap, HeapMap))
parseBoxesHeap = generalParseBoxes runState

generalParseBoxes :: Num t =>
     (PrintState [[VisObject]] -> (t, HeapMap, HeapMap) -> b)
  -> [(Box, String)] -> IO b
generalParseBoxes f bs = walkHeapSimply bs >>= \h -> walkHeapWithBCO bs >>= \h2 -> return $ f (go bs) (0,h,h2)
  where go ((b',_):b's) = do (_,h,_) <- get
                             let (Just (_,c)) = lookup b' h
                             r <- parseClosure b' c
                             rs <- go b's
                             --return (r:rs)
                             return (simplify r:rs)
        go [] = return []

-- Pulls together multiple Unnamed objects to one
simplify :: [VisObject] -> [VisObject]
simplify [] = []
simplify [Named a bs] = [Named a $ simplify bs]
simplify [a] = [a]
simplify (Unnamed a : Unnamed b : xs) = simplify $ Unnamed (a ++ b) : xs
simplify (Named a bs : xs) = Named a (simplify bs) : simplify xs
simplify (a:xs) = a : simplify xs

parseClosure :: Box -> Closure -> PrintState [VisObject]
parseClosure b c = do
  o <- correctObject b
  case o of
    Link n -> return [Link n] -- Don't build infinite heaps
    _      -> do i <- parseInternal b c
                 return $ insertObjects o i

fromJust1 :: String -> Maybe t -> t
fromJust1 _ (Just n) = n
fromJust1 x _ = error $ "Invalid fromJust " ++ x

correctObject :: Box -> PrintState VisObject
correctObject box = do
  r <- countReferences box
  n <- getName box

  case n of
    Just name -> return $ Link name
    Nothing -> if r > 1 then
                 (do setName box
                     name <- liftM (fromJust1 "1") $ getName box
                     return $ Named name [])
                 else return $ Unnamed ""

insertObjects :: VisObject -> [VisObject] -> [VisObject]
insertObjects _ xs@(Function _ : _) = xs
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
pointersToFollow (BCOClosure StgInfoTable{} _ _ _ _ _ _) = return []

pointersToFollow (MutArrClosure StgInfoTable{} _ _ bPtrs) =
  do cPtrs <- mapM getBoxedClosureData bPtrs
     return $ fix $ zip bPtrs cPtrs
  where fix ((_,ConsClosure StgInfoTable{} _ _ _ "ByteCodeInstr" "BreakInfo"):_:_:xs) = fix xs
        fix ((_,ConsClosure StgInfoTable{} _ _ _ "ByteCodeInstr" "BreakInfo"):_:xs) = fix xs
        fix ((_,ConsClosure StgInfoTable{} _ _ _ "ByteCodeInstr" "BreakInfo"):xs) = fix xs
        fix ((x,_):xs) = x : fix xs
        fix [] = []

pointersToFollow x = return $ allPtrs x

-- | Follows 'GHC.HeapView.BCOClosure's, but not the debugging data structures
--   (ByteCodeInstr.BreakInfo) of GHC.
pointersToFollow2 :: Closure -> IO [Box]
pointersToFollow2 (MutArrClosure StgInfoTable{} _ _ bPtrs) =
  do cPtrs <- mapM getBoxedClosureData bPtrs
     return $ fix $ zip bPtrs cPtrs
  where fix ((_,ConsClosure StgInfoTable{} _ _ _ "ByteCodeInstr" "BreakInfo"):_:_:xs) = fix xs
        fix ((_,ConsClosure StgInfoTable{} _ _ _ "ByteCodeInstr" "BreakInfo"):_:xs) = fix xs
        fix ((_,ConsClosure StgInfoTable{} _ _ _ "ByteCodeInstr" "BreakInfo"):xs) = fix xs
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

adjust :: (HeapEntry -> HeapEntry) -> Box -> HeapMap -> HeapMap
adjust f b h = h1 ++ ((b,f x) : h2)
  where i = fromJust1 "2" $ findIndex (\(y,_) -> y == b) h
        (h1,(_,x):h2) = splitAt i h

setName :: Box -> PrintState ()
setName b = modify go
  where go (i,h,h2) = (i + 1, adjust (set i) b h, h2)
        set i (Nothing, closure) = (Just ('t' : show i), closure)
        set _ _ = error "unexpected pattern"

getName :: Box -> PrintState (Maybe String)
getName b = do (_,h,_) <- get
               return $ fst $ fromJust1 "3" $ lookup b h

getSetName :: Box -> PrintState String
getSetName b = do mn <- getName b
                  case mn of
                    Nothing   -> do setName b
                                    n <- getName b
                                    return $ fromJust1 "4" n
                    Just name -> return name

-- How often is a box referenced in the entire heap map
countReferences :: Box -> PrintState Int
countReferences b = do
  (_,_,h) <- get
  return $ sum $ map countR h
 where countR (_,(_,c)) = length $ filter (== b) $ allPtrs c

parseInternal :: Box -> Closure -> PrintState [VisObject]
parseInternal _ (ConsClosure StgInfoTable{} _ [dataArg] _pkg modl name) =
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

    (_,name') -> printf "%s[%d]" name' dataArg
  ]

-- Empty ByteStrings point to a nullForeignPtr, evaluating it leads to an
-- Prelude.undefined exception
parseInternal _ (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return [Unnamed "ByteString[0,0]()"]

parseInternal _ (ConsClosure (StgInfoTable 1 3 _ 0) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- contParse bPtr
       return $ Unnamed (printf "ByteString[%d,%d](" start end) : cPtr ++ [Unnamed ")"]

parseInternal _ (ConsClosure (StgInfoTable 2 3 _ 1) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- contParse bPtr1
       cPtr2 <- contParse bPtr2
       return $ Unnamed (printf "Chunk[%d,%d](" start end) : cPtr1 ++ [Unnamed ","] ++ cPtr2 ++ [Unnamed ")"]

parseInternal _ (ConsClosure (StgInfoTable 1 0 CONSTR_1_0 _) [bPtr] [] _ _ name)
  = do cPtr <- contParse bPtr
       return $ Unnamed (name ++ " ") : mbParens cPtr

parseInternal _ (ConsClosure (StgInfoTable 0 0 CONSTR_NOCAF_STATIC _) [] [] _ _ name)
  = return [Unnamed name]

parseInternal _ (ConsClosure (StgInfoTable 0 _ CONSTR_NOCAF_STATIC _) [] args _ _ name)
  = return [Unnamed (name ++ " " ++ show args)]

parseInternal _ (ConsClosure (StgInfoTable 2 0 _ 1) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- liftM mbParens $ contParse bHead
       cTail <- liftM mbParens $ contParse bTail
       return $ cHead ++ [Unnamed ":"] ++ cTail

parseInternal _ (ConsClosure (StgInfoTable _ 0 _ _) bPtrs [] _ _ name)
  = do cPtrs <- mapM (liftM mbParens . contParse) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       return $ Unnamed (name ++ " ") : tPtrs

parseInternal _ (ConsClosure (StgInfoTable _ _ _ _) bPtrs dArgs _ _ name)
  = do cPtrs <- mapM (liftM mbParens . contParse) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       return $ Unnamed (name ++ show dArgs ++ " ") : tPtrs

parseInternal _ (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) _ arrWords)
  = return $ intercalate [Unnamed ","] (map (\x -> [Unnamed (printf "0x%x" x)]) arrWords)

parseInternal _ (IndClosure (StgInfoTable 1 0 _ 0) b)
  = contParse b

parseInternal _ (SelectorClosure StgInfoTable{} b)
  = contParse b

parseInternal _ (BlackholeClosure (StgInfoTable 1 0 _ 0) b)
  = contParse b

-- Reversed order of ptrs
parseInternal b (ThunkClosure StgInfoTable{} bPtrs args)
  = parseThunkFun b bPtrs args

parseInternal b (FunClosure StgInfoTable{} bPtrs args)
  = parseThunkFun b bPtrs args

-- bPtrs here can currently point to Nothing, because else we might get infinite heaps
parseInternal _ (MutArrClosure StgInfoTable{} _ _ bPtrs)
  = do cPtrs <- mutArrContParse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ Unnamed "(" : tPtrs ++ [Unnamed ")"]

parseInternal _ (BCOClosure (StgInfoTable 4 0 BCO 0) _ _ bPtr _ _ _)
  = do cPtrs <- bcoContParse [bPtr]
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ Unnamed "(" : tPtrs ++ [Unnamed ")"]
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

parseInternal b (APClosure (StgInfoTable 0 0 _ _) _ _ fun pl)
  = parseAPPAP b fun pl

parseInternal b (PAPClosure (StgInfoTable 0 0 _ _) _ _ fun pl)
  = parseAPPAP b fun pl

parseInternal _ (MVarClosure StgInfoTable{} qHead qTail qValue)
   = do cHead <- liftM mbParens $ contParse qHead
        cTail <- liftM mbParens $ contParse qTail
        cValue <- liftM mbParens $ contParse qValue
        return $ Unnamed "MVar#(" : cHead ++ [Unnamed ","] ++ cTail ++ [Unnamed ","] ++ cValue ++ [Unnamed ")"]

parseInternal _ c = return [Unnamed ("Missing pattern for " ++ show c)]

-- λ> data BinaryTree = BT BinaryTree Int BinaryTree | Leaf deriving Show
-- λ> let x = BT (BT (BT Leaf 1 (BT Leaf 2 Leaf)) 3 (BT (BT Leaf 4 (BT Leaf 5 Leaf)) 6 Leaf))
-- λ> :view x (in list view)
parseAPPAP :: Box -> Box -> [Box] -> PrintState [VisObject]
parseAPPAP b fun pl = do
  name <- getSetName b
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
  return $ Function name : fPtr ++ [Unnamed "["] ++ tPtrs ++ [Unnamed "]"]

parseThunkFun :: Box -> [Box] -> [Word] -> PrintState [VisObject]
parseThunkFun b bPtrs args = do
  name <- getSetName b
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
  return $ if null args then
    Function name : Unnamed "(" : tPtrs ++ [Unnamed ")"] else
    Function name : (Unnamed $ show args ++ "(") : tPtrs ++ [Unnamed ")"]

contParse :: Box -> PrintState [VisObject]
contParse b = get >>= \(_,h,_) -> parseClosure b (snd $ fromJust1 "5" $ lookup b h)

bcoContParse :: [Box] -> PrintState [[VisObject]]
bcoContParse [] = return []
bcoContParse (b:bs) = get >>= \(_,h,_) -> case lookup b h of
  Nothing    -> do let ptf = unsafePerformIO $ getBoxedClosureData b >>= pointersToFollow2
                   bcoContParse $ ptf ++ bs -- Could go into infinite loop
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- bcoContParse bs
                   return $ p : ps

mutArrContParse :: [Box] -> PrintState [[VisObject]]
mutArrContParse [] = return []
mutArrContParse (b:bs) = get >>= \(_,h,_) -> case lookup b h of
  Nothing -> mutArrContParse bs
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- mutArrContParse bs
                   return $ p : ps

-- TODO: Doesn't work quite right, for example with (1,"fo")
mbParens :: [VisObject] -> [VisObject]
mbParens t@(Unnamed ('"':_):_) = t
mbParens t@(Unnamed ('(':_):_) = t
mbParens t | ' ' `objElem` t = Unnamed "(" : t ++ [Unnamed ")"]
            | otherwise     = t
  where objElem c = any go
          where go (Unnamed xs) = c `elem` xs
                go (Named _ os) = any go os
                go _ = False

-- | Textual representation of Heap objects, used in the graph visualization.
showClosure :: Closure -> String
showClosure (ConsClosure StgInfoTable{} _ [dataArg] _ modl name) =
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

    -- :m +GHC.Arr
    -- let b = array ((1,1),(3,2)) [((1,1),42),((1,2),23),((2,1),999),((2,2),1000),((3,1),1001),((3,2),1002)]
    -- b
    -- :view b
    ("GHC.Arr", "Array") -> printf "Array[%d]" dataArg

    (_,name') -> printf "%s[%d]" name' dataArg

showClosure (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = "ByteString[0,0]"

showClosure (ConsClosure (StgInfoTable 1 3 _ 0) [_] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = printf "ByteString[%d,%d]" start end

showClosure (ConsClosure (StgInfoTable 2 3 _ 1) [_,_] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = printf "Chunk[%d,%d]" start end

showClosure (ConsClosure StgInfoTable{} _ [] _ _ name)
  = name

showClosure (ConsClosure StgInfoTable{} _ dArgs _ _ name)
  = name ++ show dArgs

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

showClosure (ArrWordsClosure (StgInfoTable 0 0 ARR_WORDS 0) _ arrWords)
  = intercalate ",\n" $ map (printf "0x%x") arrWords
--  = "ArrWords"

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

showClosure OtherClosure{}
  = "Other"

showClosure c = "Missing pattern for " ++ show c
