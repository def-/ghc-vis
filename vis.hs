import GHC.HeapView
import Control.Monad
import Data.Tree

--buildTree :: a -> (Closure, [Closure])
--buildTree x = (y, [mapM buildTree $ liftM allPtrs y])
--  where y = getBoxedClosureData x
buildTree x = do
  cd <- getBoxedClosureData x
  return (cd, allPtrs cd)

t x = unfoldTreeM buildTree x
