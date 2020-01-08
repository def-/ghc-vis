{-# LANGUAGE CPP, MagicHash, DeriveDataTypeable, NoMonomorphismRestriction, RankNTypes, RecordWildCards #-}

{- |
   Module      : GHC.Vis.Internal
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Internal (
  parseClosure,
  showClosureFields
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import GHC.Vis.Types

import GHC.HeapView hiding (pkg, modl, fun, arrWords)

import Control.Monad
import Control.Monad.State.Strict hiding (State, fix)

import Data.Char
import Data.List hiding (insert)
import qualified Data.IntMap as M

import Text.Printf
import Unsafe.Coerce

import System.IO.Unsafe

-- TODO: Remove
instance Eq Box where
  a == b = unsafePerformIO $ areBoxesEqual a b
  {-# NoINLINE (==) #-}

-- | Parse a closure to a list of VisObjects
parseClosure :: HeapGraphIndex -> PrintState [VisObject]
parseClosure i = do
  o <- correctObject i
  case o of
    Link n -> return [Link n] -- Don't build infinite heaps
    _      -> do HeapGraph m <- gets heapGraph
                 l <- parseInternal i $ hgeClosure $ m M.! i
                 return $ insertObjects o l

correctObject :: HeapGraphIndex -> PrintState VisObject
correctObject i = do
  name <- getName i
  bindings <- gets bindings

  if null name
  then if i `elem` bindings then
                 (do name' <- setName i
                     return $ Named name' [])
                 else return $ Unnamed ""
  else return $ Link name

setName :: HeapGraphIndex -> PrintState String
setName i = do
  n <- getName i
  if null n
  then do
    s@(PState ti fi xi _ (HeapGraph m)) <- get
    let Just hge@(HeapGraphEntry{hgeClosure = c}) = M.lookup i m
    let (name,newState) = case bindingLetter c of
          't' -> ('t' : show ti, s{tCounter' = ti + 1})
          'f' -> ('f' : show fi, s{fCounter' = fi + 1})
          'x' -> ('x' : show xi, s{xCounter' = xi + 1})
          _   -> error "Invalid letter"

    let m' = M.insert i (hge{hgeData = name}) m
    put $ newState{heapGraph = HeapGraph m'}
    return name
  else return n

  where
    bindingLetter c = case c of
        ThunkClosure {..} -> 't'
        SelectorClosure {..} -> 't'
        APClosure {..} -> 't'
        PAPClosure {..} -> 'f'
        BCOClosure {..} -> 't'
        FunClosure {..} -> 'f'
        _ -> 'x'

getName :: HeapGraphIndex -> PrintState String
getName i = do
  HeapGraph m <- gets heapGraph
  return $ hgeData $ m M.! i

insertObjects :: VisObject -> [VisObject] -> [VisObject]
insertObjects _ xs@(Function _ : _) = xs
insertObjects _ xs@(Thunk _ : _) = xs
insertObjects (Link name) _ = [Link name]
insertObjects (Named name _) xs = [Named name xs]
insertObjects (Unnamed _) xs = xs
insertObjects _ _ = error "unexpected arguments"

parseInternal :: HeapGraphIndex -> GenClosure (Maybe HeapGraphIndex) -> PrintState [VisObject]

-- TODO remove old cases replaced by closure types
parseInternal _ (ConstrClosure _ [] [dataArg] _pkg modl name) =
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
                 ] -> name ++ " " ++ show ((fromIntegral :: Word -> Int) dataArg)

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg
    --("GHC.Types", "C#") -> '\'' : (chr $ fromIntegral dataArg) : "'"

    ("GHC.Types", "D#") -> printf "D# %0.5f" (unsafeCoerce dataArg :: Double)
    ("GHC.Types", "F#") -> printf "F# %0.5f" (unsafeCoerce dataArg :: Double)

    _ -> printf "%s %d" (infixFix name) dataArg
  ]

parseInternal _ (IntClosure _ val) = return [Unnamed $ "I# " ++ show val]

parseInternal _ (Int64Closure _ val) = return [Unnamed $ "I64# " ++ show val]

parseInternal _ (WordClosure _ val) = return [Unnamed $ "W# " ++ show val]

parseInternal _ (Word64Closure _ val) = return [Unnamed $ "W64# " ++ show val]

parseInternal _ (FloatClosure _ val) = return [Unnamed $ printf "F# %0.5f" val]

parseInternal _ (DoubleClosure _ val) = return [Unnamed $ printf "D# %0.5f" val]

parseInternal _ (AddrClosure _ val) = return [Unnamed $ printf "Addr# %p" val]

parseInternal _ (ConstrClosure (StgInfoTable _ 1 3 _ _ _) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return [Unnamed "ByteString 0 0"]

parseInternal _ (ConstrClosure (StgInfoTable _ 1 3 _ _ _) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- liftM mbParens $ contParse bPtr
       return $ Unnamed (printf "ByteString %d %d " start end) : cPtr

parseInternal _ (ConstrClosure (StgInfoTable _ 2 3 _ _ _) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- liftM mbParens $ contParse bPtr1
       cPtr2 <- liftM mbParens $ contParse bPtr2
       return $ Unnamed (printf "Chunk %d %d " start end) : cPtr1 ++ [Unnamed " "] ++ cPtr2

parseInternal _ (ConstrClosure (StgInfoTable _ 2 0 _ _ _) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- liftM mbParens $ contParse bHead
       cTail <- liftM mbParens $ contParse bTail
       return $ cHead ++ [Unnamed ":"] ++ cTail

parseInternal _ (ConstrClosure _ bPtrs dArgs _ _ name)
  = do cPtrs <- mapM (liftM mbParens . contParse) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       let sPtrs = if null tPtrs then [Unnamed ""] else Unnamed " " : tPtrs
       return $ Unnamed (unwords $ infixFix name : map show dArgs) : sPtrs

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

parseInternal _ (OtherClosure (StgInfoTable _ _ _ cTipe _ _) _ _)
  = return [Unnamed $ show cTipe]

parseInternal _ (UnsupportedClosure (StgInfoTable _ _ _ cTipe _ _))
  = return [Unnamed $ show cTipe]

-- Reversed order of ptrs
parseInternal b (ThunkClosure _ bPtrs args) = do
  name <- setName b
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
      sArgs = Unnamed $ if null args then "" else show args
  return $ Thunk (infixFix name) : sPtrs ++ [sArgs]

parseInternal i (FunClosure _ bPtrs args) = do
  name <- setName i
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
      sArgs = Unnamed $ if null args then "" else show args
  return $ Function (infixFix name) : sPtrs ++ [sArgs]

-- bPtrs here can currently point to Nothing, because else we might get infinite heaps
parseInternal _ (MutArrClosure _ _ _ bPtrs)
  -- = do cPtrs <- mutArrContParse2 bPtrs
  = do cPtrs <- mapM contParse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]

parseInternal _ (MutVarClosure _ b)
  = do c <- contParse b
       return $ Unnamed "MutVar " : c

parseInternal _ (BCOClosure _ _ _ bPtr _ _ _)
  -- = do cPtrs <- bcoContParse2 [bPtr]
  = do cPtrs <- mapM contParse [bPtr]
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ Unnamed "BCO" : tPtrs

parseInternal i (APClosure _ _ _ fun pl) = do
  name <- setName i
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Thunk (infixFix name) : fPtr ++ sPtrs

parseInternal i (PAPClosure _ _ _ fun pl) = do
  name <- setName i
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Function (infixFix name) : fPtr ++ sPtrs

parseInternal i (APStackClosure _ fun pl) = do
  name <- setName i
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Thunk (infixFix name) : fPtr ++ sPtrs

parseInternal _ (MVarClosure _ qHead qTail qValue)
   = do cHead <- liftM mbParens $ contParse qHead
        cTail <- liftM mbParens $ contParse qTail
        cValue <- liftM mbParens $ contParse qValue
        return $ Unnamed "MVar#(" : cHead ++ [Unnamed ","] ++ cTail ++ [Unnamed ","] ++ cValue ++ [Unnamed ")"]

contParse :: Maybe HeapGraphIndex -> PrintState [VisObject]
contParse Nothing = return []
contParse (Just i) = parseClosure i

-- It turned out that bcoContParse actually does go into an infinite loop, for
-- example for this:
-- foldr' op i [] = i
-- foldr' op i (x:xs) = op x (foldr' op i xs)
-- :view foldr'
-- We fix this by giving visited closures a dummy name, so we recognize when we
-- get into a loop.
--bcoContParse :: [Box] -> MaybeT PrintState [[VisObject]]
--bcoContParse [] = return []
--bcoContParse (b:bs) = gets heapMap >>= \h -> case lookup b h of
--  Nothing    -> do let ptf = unsafePerformIO $ getBoxedClosureData b >>= pointersToFollow2
--                   r <- lift $ countReferences b
--                   n <- getName b
--                   case n of
--                     Just _ -> bcoContParse bs
--                     Nothing -> do
--                       when (r > 1) $ setVisited b
--                       bcoContParse $ ptf ++ bs
--  Just (_,c) -> do p  <- parseClosure b c
--                   ps <- bcoContParse bs
--                   return $ p : ps

--mutArrContParse :: [Box] -> MaybeT PrintState [[VisObject]]
--mutArrContParse [] = return []
--mutArrContParse (b:bs) = gets heapMap >>= \h -> case lookup b h of
--  Nothing -> mutArrContParse bs
--  Just (_,c) -> do p  <- parseClosure b c
--                   ps <- mutArrContParse bs
--                   return $ p : ps

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

--showClosure :: Closure -> String
--showClosure = unwords . showClosureFields

-- | Textual representation of Heap objects, used in the graph visualization.
showClosureFields :: GenClosure t -> [String]
showClosureFields (ConstrClosure _ _ [dataArg] _ modl name) =
 case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> [name, show dataArg]

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> [name, show ((fromIntegral :: Word -> Int) dataArg)]

    ("GHC.Types", "C#") -> ["C#", [chr $ fromIntegral dataArg]]

    ("GHC.Types", "D#") -> ["D#", printf "%0.5f" (unsafeCoerce dataArg :: Double)]
    ("GHC.Types", "F#") -> ["F#", printf "%0.5f" (unsafeCoerce dataArg :: Double)]

    -- :m +GHC.Arr
    -- let b = array ((1,1),(3,2)) [((1,1),42),((1,2),23),((2,1),999),((2,2),1000),((3,1),1001),((3,2),1002)]
    -- b
    -- :view b
    _ -> [name, show dataArg]

showClosureFields (IntClosure _ val) = ["I# " ++ show val]

showClosureFields (Int64Closure _ val) = ["I64# " ++ show val]

showClosureFields (WordClosure _ val) = ["W# " ++ show val]

showClosureFields (Word64Closure _ val) = ["W64# " ++ show val]

showClosureFields (FloatClosure _ val) = [printf "F# %0.5f" val]

showClosureFields (DoubleClosure _ val) = [printf "D# %0.5f" val]

showClosureFields (AddrClosure _ val) = [printf "Addr# %p" val]

showClosureFields (ConstrClosure (StgInfoTable _ 1 3 _ 0 _) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = ["ByteString","0","0"]

showClosureFields (ConstrClosure (StgInfoTable _ 1 3 _ 0 _) [_] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = ["ByteString",printf "%d" start,printf "%d" end]

showClosureFields (ConstrClosure (StgInfoTable _ 2 3 _ 1 _) [_,_] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = ["Chunk",printf "%d" start,printf "%d" end]

showClosureFields (ConstrClosure _ _ dArgs _ _ name)
  = name : map show dArgs

-- Reversed order of ptrs
showClosureFields ThunkClosure{}
  = ["Thunk"]

showClosureFields SelectorClosure{}
  = ["Selector"]

-- Probably should delete these from Graph
showClosureFields IndClosure{}
  = ["Ind"]

showClosureFields BlackholeClosure{}
  = ["Blackhole"]

showClosureFields APClosure{}
  = ["AP"]

showClosureFields PAPClosure{}
  = ["PAP"]

showClosureFields APStackClosure{}
  = ["APStack"]

showClosureFields BCOClosure{}
  = ["BCO"]

showClosureFields (ArrWordsClosure _ _ arrWords)
  = map (printf "0x%x") arrWords

showClosureFields MutArrClosure{}
  = ["MutArr"]

showClosureFields MutVarClosure{}
  = ["MutVar"]

showClosureFields MVarClosure{}
  = ["MVar#"]

showClosureFields FunClosure{}
  = ["Fun"]

showClosureFields BlockingQueueClosure{}
  = ["BlockingQueue"]

showClosureFields (OtherClosure (StgInfoTable _ _ _ cTipe _ _) _ _)
  = [show cTipe]

showClosureFields (UnsupportedClosure (StgInfoTable _ _ _ cTipe _ _))
  = [show cTipe]

--showClosure c = "Missing pattern for " ++ show c

-- | Make infix names prefix
infixFix :: String -> String
infixFix xs
  | isInfix xs = '(' : xs ++ ")"
  | otherwise  = xs

-- | Determine whether a name is an infix name, based on
-- http://www.haskell.org/onlinereport/haskell2010/haskellch2.html
isInfix :: String -> Bool
isInfix []              = False
isInfix ('[':_)         = False
isInfix ('(':_)         = False
isInfix (x:_)
  | x `elem` ascSymbols = True
  | isSymbol x          = True
  | isPunctuation x     = True
  | otherwise           = False
  where ascSymbols = ("!#$%&*+./<=>?@  \\^|-~:" :: String)
