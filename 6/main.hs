import Data.List.Split
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Maybe (isJust, fromJust)

allOrbitsNum :: [(String, String)] -> Int
allOrbitsNum pairs = sum $ M.elems $ orbitsNums
  where count pl = sum [1 + M.findWithDefault 0 npl orbitsNums | npl <- direct M.! pl]
        orbitsNums = M.fromList [(pl, count pl) | pl <- M.keys direct]
        direct = foldr (\(k, v) -> M.alter (Just . maybe [v] (v :)) k) M.empty pairs

minDistance :: [(String, String)] -> Int
minDistance pairs = dist + parentsM M.! commonParent
  where invM = M.fromList $ map swap pairs
        parentsM = M.fromList $ zip parents [0..] 
        parents = map fromJust $ takeWhile isJust 
                $ iterate (>>= (`M.lookup` invM)) $ Just (invM M.! "SAN")
        (dist, commonParent) = head $ dropWhile ((`M.notMember` parentsM) . snd) 
                  $ zip [0..] $ iterate (invM M.!) $ invM M.! "YOU"

main = do
  ls <- map (splitOn ")") . lines <$> readFile "in"
  let m = [(f, t) | [f, t] <- ls]
  print $ allOrbitsNum m
  print $ minDistance m