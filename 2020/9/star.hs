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
  let Just code = invalidCode size codes
  print $ weakness code (0, IndexedList 0 codes, IndexedList 0 codes)

invalidCode :: Int -> [Int] -> Maybe Int
invalidCode size codes = fmap fst $ listToMaybe 
    $ dropWhile validCode $ zip newCodes $ scanl delAddCodes preamble 
    $ zip newCodes oldCodes
  where preamble = S.fromList $ take size codes
        newCodes = drop size codes
        oldCodes = codes
        delAddCodes preamble (nc, oc) = S.insert nc $ S.delete oc preamble
        validCode (nc, preamble) = not $ S.null 
          $ S.filter (\x -> x < div (nc+1) 2 && 
                            S.member (nc-x) preamble) preamble

data IndexedList = IndexedList Int [Int]
  deriving (Show)

headI :: IndexedList -> Int
headI (IndexedList _ xs) = head xs

tailI :: IndexedList -> IndexedList
tailI (IndexedList ind xs) = IndexedList (ind+1) (tail xs)

diff :: IndexedList -> IndexedList -> [Int]
diff (IndexedList ind1 xs) (IndexedList ind2 _) = take (ind2-ind1) xs

weakness :: Int -> (Int, IndexedList, IndexedList) -> Int
weakness target (sum, subs, adds)
  | sum' == target = let xs = diff subs' adds' in maximum xs + minimum xs
    | otherwise      = weakness target (sum', subs', adds')
  where (sum', subs', adds')
           | sum < target = (sum + headI adds, subs, tailI adds)
           | sum > target = (sum - headI subs, tailI subs, adds)
           | otherwise = error "sum and target should not be equal"
