module Main where

#if __GLASGOW_HASKELL__ == 704
import qualified GhciVis704.Main as G
#else
import qualified GhciVis706.Main as G
#endif

main :: IO ()
main = G.main
