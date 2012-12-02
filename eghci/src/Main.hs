module Main where

#if __GLASGOW_HASKELL__ == 704
import qualified Eghci704.Main as G
#else
import qualified Eghci706.Main as G
#endif

main :: IO ()
main = G.main
