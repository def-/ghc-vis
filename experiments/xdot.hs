import Data.GraphViz.Types
import Data.Text.Lazy
import Data.GraphViz.Types.Generalised

parseXDot :: FilePath -> IO (DotGraph String)
parseXDot x = do
  f <- readFile x
  let fs = pack f

  return $ parseDotGraph fs
