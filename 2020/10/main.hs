import Control.Monad (when)
import Data.List (sort)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  joltages <- sort . map read . lines <$> readFile (head args)
  let diffs = (++ [3]) $ zipWith (-) joltages $ 0:joltages
  print $ (length $ filter (== 1) diffs) * (length $ filter (== 3) diffs)
