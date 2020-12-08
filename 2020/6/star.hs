import Control.Monad (when)
import System.Environment (getArgs)

import Data.List.Split (splitOn)

import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  answers <- map lines . splitOn "\n\n" <$> readFile (head args)
  print $ sum $ map (S.size . foldl1 S.intersection . map S.fromList) answers
