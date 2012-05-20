{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.Vis
import GHC.Exts

import Control.DeepSeq

import System.Environment
import System.Mem

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

-- Usage in GHCi:
-- λ> let a = cycle "bar"
-- λ> printP a
-- t0
-- λ> evalP a "t0"
-- t0=('b':t1(t2[0](t3[51600224]()),t0))
-- λ> evalP a "t2"
-- t0=('b':t1('a':t3[1](t4[51600224]()),t0))
-- λ> evalP a "t1"
-- t0=('b':'a':t2(t3[1](t4[51600224]()),t0))
-- λ> evalP a "t2"
-- t0=('b':'a':'r':t3(t4[2](t5[51600224]()),t0))
-- λ> evalP a "t4"
-- t0=('b':'a':'r':t3([],t0))
-- λ> evalP a "t3"
-- t0=('b':'a':'r':t0)

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
  b `evalS` "t0" >>= putStrInd
  putStrLn "Evaluate t1:"
  b `evalS` "t1" >>= putStrInd
  putStrLn "Evaluate t3:"
  b `evalS` "t3" >>= putStrInd
  putStrLn "Evaluate t4:"
  b `evalS` "t4" >>= putStrInd
  putStrLn "Evaluate t5:"
  b `evalS` "t5" >>= putStrInd
  putStrLn "Evaluate t6:"
  b `evalS` "t6" >>= putStrInd
  putStrLn "Evaluate t2:"
  b `evalS` "t2" >>= putStrInd
  putStrLn "Evaluate t3:"
  b `evalS` "t3" >>= putStrInd
  putStrLn "Evaluate t4:"
  b `evalS` "t4" >>= putStrInd
  putStrLn "Evaluate t5:"
  b `evalS` "t5" >>= putStrInd

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
  l2 `evalS` "t0" >>= putStrInd
  putStrLn "Evaluate t1:"
  l2 `evalS` "t1" >>= putStrInd
  putStrLn "Evaluate t0:"
  l2 `evalS` "t0" >>= putStrInd

putStrInd x = putStrLn $ "  " ++ x
