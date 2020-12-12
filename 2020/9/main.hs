import Control.Monad (when)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  size:codes <- map read . lines <$> readFile (head args)
  let preamble = S.fromList $ take size codes
      newCodes = drop size codes
      oldCodes = codes
      delAddCodes preamble (nc, oc) = S.insert nc $ S.delete oc preamble
      validCode (nc, preamble) = not $ S.null 
        $ S.filter (\x -> x < div (nc+1) 2 && S.member (nc-x) preamble) preamble
  maybe (putStrLn "No invalid codes") (print . fst) $ listToMaybe 
    $ dropWhile validCode $ zip newCodes $ scanl delAddCodes preamble 
    $ zip newCodes oldCodes
