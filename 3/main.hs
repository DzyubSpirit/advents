import           Data.List.Split                ( splitOn )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Tuple                     ( swap )
import           Data.Function                  ( on )

import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M

type Point = (Int, Int)

points :: String -> [Point]
points = fst . foldl move' ([], (0, 0)) . splitOn ","
  where move' (xs, p) d = let ps = move d p in (xs ++ ps, last ps)

pointDists :: [Point] -> M.Map Point Int
pointDists ps = M.fromList $ reverse $ zip ps [1 ..]

move :: String -> Point -> [Point]
move str p = take n $ drop 1 $ iterate f p
 where
  n = read $ tail str
  f = case head str of
    'R' -> first (+ 1)
    'L' -> first (\x -> x - 1)
    'U' -> second (+ 1)
    'D' -> second (\x -> x - 1)

main :: IO ()
main = do
  [w1, w2] <- lines <$> readFile "in"
  let ps1 = points w1
  let ps2 = points w2
  print $ minimum $ map (uncurry ((+) `on` abs)) $ S.toList $ S.intersection
    (S.fromList ps1)
    (S.fromList ps2)
  print $ minimum $ M.elems $ M.intersectionWith (+)
                                                 (pointDists ps1)
                                                 (pointDists ps2)
