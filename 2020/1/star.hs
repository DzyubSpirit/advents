import Control.Monad (when)
import Data.Bifunctor (bimap)
import System.Environment (getArgs)

import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  coins <- map read . lines <$> readFile (head args) :: IO [Int]
  let coinSet = S.fromList coins
      result = safeHead [ c1*c2*c3
                        | c1 <- coins
                        , c2 <- coins
                        , c1 /= c2
                        , let c3 = 2020-c1-c2
                        , c3 /= c1 
                        , c3 /= c2
                        , c3 `S.member` coinSet]
  maybe (putStrLn "No suitable combinations") print result

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing
