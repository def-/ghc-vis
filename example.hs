{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.Vis
import GHC.Exts

import Control.DeepSeq

import System.Environment
import System.Mem

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

l = [1,2,3]

main = do
  putStrLn "Examples from ghc-heap-view"
  args <- map length `fmap` getArgs
  let l2 = 4:l
  (l ++ l2 ++ args) `deepseq` return ()
  let x = l ++ l2 ++ args
  performGC

  bprint l >>= putStrInd
  bprint l2 >>= putStrInd
  bprint args >>= putStrInd
  bprint [] >>= putStrInd
  bprint x >>= putStrInd

  let !(I# m) = length args + 42
  let !(I# m') = length args + 23
  let f = \x n -> take (I# m + I# x) n ++ args
      t = f m' l2

  bprint f >>= putStrInd
  bprint t >>= putStrInd

  let x = id (:) () x
  x `seq` return ()
  performGC
  bprint x >>= putStrInd

  let a = [1..5]
  let b = a++a

  putStrLn "\nFully evaluating b step by step"
  bprint b >>= putStrInd
  putStrLn "Evaluate t0:"
  dprint b >>= eval3 "t0"
  bprint b >>= putStrInd
  putStrLn "Evaluate t1:"
  dprint b >>= eval3 "t1"
  bprint b >>= putStrInd
  putStrLn "Evaluate t3:"
  dprint b >>= eval3 "t3"
  bprint b >>= putStrInd
  putStrLn "Evaluate t4:"
  dprint b >>= eval3 "t4"
  bprint b >>= putStrInd
  putStrLn "Evaluate t5:"
  dprint b >>= eval3 "t5"
  bprint b >>= putStrInd
  putStrLn "Evaluate t6:"
  dprint b >>= eval3 "t6"
  bprint b >>= putStrInd
  putStrLn "Evaluate t2:"
  dprint b >>= eval3 "t2"
  bprint b >>= putStrInd
  putStrLn "Evaluate t3:"
  dprint b >>= eval3 "t3"
  bprint b >>= putStrInd
  putStrLn "Evaluate t4:"
  dprint b >>= eval3 "t4"
  bprint b >>= putStrInd
  putStrLn "Evaluate t5:"
  dprint b >>= eval3 "t5"
  bprint b >>= putStrInd

  putStrLn "\nByteStrings"
  let b = B.empty
  let b2 = B.pack [70,71,72,73]
  bprint b >>= putStrInd
  b2 `seq` return ()
  bprint b2 >>= putStrInd

  putStrLn "\nLazy ByteStrings"
  let l = LB.empty
  let l2 = LB.pack [70,71,72,73]
  bprint l >>= putStrInd
  bprint l2 >>= putStrInd
  putStrLn "Evaluate t0:"
  dprint l2 >>= eval3 "t0"
  bprint l2 >>= putStrInd
  putStrLn "Evaluate t1:"
  dprint l2 >>= eval3 "t1"
  bprint l2 >>= putStrInd
  putStrLn "Evaluate t0:"
  dprint l2 >>= eval3 "t0"
  bprint l2 >>= putStrInd

putStrInd x = putStrLn $ "  " ++ x
