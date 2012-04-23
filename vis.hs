import GHC.HeapView
import Control.Monad
import Data.Tree

buildTree x = do
  cd <- getBoxedClosureData x
  return (cd, allPtrs cd)

t x = unfoldTreeM buildTree x
u = t . asBox

-- infinite tree duh
f = let y = id (:) () y
    in getClosureData y

