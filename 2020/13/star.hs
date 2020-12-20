import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  startStr:idsStr:_ <- lines <$> readFile (head args)
  let start = read startStr :: Int 
      ((_, id0):dtAndIds) = map (fmap read) $ filter ((/= "x") . snd) 
          $ zip [0..] $ splitOn "," idsStr 
      (k, _) = foldl (findK id0) (1, 1) dtAndIds
  print $ id0 * k

type KAndStep = (Int, Int)
type DtAndId  = (Int, Int)

findK :: Int -> KAndStep -> DtAndId -> KAndStep
findK id1 (init, step) (dt2, id2) = 
  (head $ filter constraint options, step * (div id2 (gcd (id1*step) id2)))
  where constraint k = (id1 * k + dt2) `mod` id2 == 0
        options = [init,init+step..]

{-

t+dt1 = id1 * k1
t+dt2 = id2 * k2
t+dt3 = id3 * k3

id1 * k1 + dt2-dt1 = id2 * k2
k2 = (id1 * k1 + dt2-dt1) / id2
id1 * k1 + dt2-dt1 :: id2

(id1%id2) * k1 + dt2-dt1 :: id2
(id1%id2)*kk :: id2
(id1%id3)*kk :: id3


(id1%id3) * k1 + dt3-dt1 :: id3

7*13=91



-}
