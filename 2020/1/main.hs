import Control.Monad (when)
import System.Environment (getArgs)

import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  coins <- map read . lines <$> readFile (head args) :: IO [Int]
  let result = fmap (\(x, _) -> x * (2020 - x))
             $ safeHead $ dropWhile (uncurry notHavingPair)
             $ zip coins
             $ scanl (flip S.insert) S.empty coins
  maybe (putStrLn "No suitable pairs") print result
  
notHavingPair :: Int -> S.Set Int -> Bool
notHavingPair coin coins = S.notMember (2020-coin) coins

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing
