import Control.Monad (when)
import qualified Data.Array as A
import Data.Bifunctor (bimap)
import System.Environment (getArgs)

type Height = Int
type Width  = Int

data TreeMap = TreeMap 
  { height :: Height
  , width :: Width 
  , arr   :: A.Array (Int, Int) Char
  }

fromString :: String -> TreeMap
fromString str = TreeMap height width 
               $ A.listArray ((0, 0), (height-1, width-1)) 
               $ filter (/= '\n') str
  where width  = length $ head $ lines str
        height = length str `div` (width+1)

(!) :: TreeMap -> (Int, Int) -> Char
(TreeMap _ width arr) ! (y, x) =  arr A.! (y, x `mod` width)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  treeMap <- fromString <$> readFile (head args)
  print $ length $ filter (== '#') $ map (treeMap !)
        $ takeWhile ((< height treeMap) . fst)
        $ iterate (bimap (+ 1) (+ 3)) (0, 0)
