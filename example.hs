{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.Vis
import GHC.Exts

import Control.DeepSeq

import System.Environment
import System.Mem

import qualified Data.ByteString as B

l = [1,2,3]

main = do
  args <- map length `fmap` getArgs
  let l2 = 4:l
  (l ++ l2 ++ args) `deepseq` return ()
  let x = l ++ l2 ++ args
  performGC

  bprint l >>= putStrLn
  bprint l2 >>= putStrLn
  bprint args >>= putStrLn
  bprint [] >>= putStrLn
  bprint x >>= putStrLn

  let !(I# m) = length args + 42
  let !(I# m') = length args + 23
  let f = \x n -> take (I# m + I# x) n ++ args
      t = f m' l2

  bprint f >>= putStrLn
  bprint t >>= putStrLn

  let x = id (:) () x
  x `seq` return ()
  performGC
  bprint x >>= putStrLn

  let b = B.empty
  let b2 = B.pack [70,71,72,73]
  bprint b >>= putStrLn
  b2 `seq` return ()
  bprint b2 >>= putStrLn
