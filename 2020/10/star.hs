import Control.Monad (when)
import Data.Function (on)
import Data.List (groupBy, sort)
import System.Environment (getArgs)

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  joltages <- map read . lines <$> readFile (head args)
  let joltMap = M.fromList $ ((maxJolt + 3, 1):)
              $ zip (0:joltages) [0,0..]
      maxJolt = maximum joltages
  let numCombs m x = M.insert x (sum $ map offNumCombs [1,2,3]) m
        where offNumCombs offset = M.findWithDefault 0 (x+offset) m
  print $ (M.! 0) $ foldl numCombs joltMap 
        $ filter (`M.member` joltMap) [maxJolt+2, maxJolt+1..0]
