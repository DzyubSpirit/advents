import           Data.List.Split                ( splitOn )
import           Data.Char                      ( ord )
import           Control.Monad                  ( liftM3 )
import           Data.Char                      ( digitToInt )
import           Data.Bifunctor                 ( bimap )
import           Data.List                      ( groupBy )

import qualified Data.Map                      as M

range = "356261-846303"
--range = "1111-11111"

type Digit = Int
type DP = M.Map (Bool, Digit, Int) Int

firstDupl :: DP -> (Bool, Digit, Int) -> Int
firstDupl _ (False, _, 0) = 0
firstDupl _ (True , _, 0) = 1
firstDupl _ (False, _, 1) = 1
firstDupl _ (True , d, 1) = 10 - d
firstDupl dp (hasDupl, d, n)
  | d + 1 > 9 = dp M.! (True, d, n - 1)
  | otherwise = dp M.! (True, d, n - 1) + dp M.! (False, d + 1, n)

passNums :: Int -> DP
passNums maxNum = dp
 where
  indicies = liftM3 (,,) [False, True] [0 .. 9] [0 .. maxNum]
  dp       = M.fromList [ (i, f i) | i <- indicies ]
  f (False, _, 0) = 0
  f (True , d, 0) = 1
  f (False, _, 1) = 0
  f (True , d, 1) = 10 - d
  f (hasDupl, d, n) =
    sum [ firstDupl dp (hasDupl, d', n - 1) | d' <- [d .. 9] ]

makeUpMono :: [Int] -> [Int]
makeUpMono []           = []
makeUpMono [d         ] = [d]
makeUpMono (x : y : xs) = if y' >= x
  then x : y' : xs'
  else x - 1 : replicate (length xs + 1) 9
  where (y' : xs') = makeUpMono (y : xs)

makeDownMono :: [Int] -> [Int]
makeDownMono [] = []
makeDownMono xs =
  head xs : zipWith (\nd pd -> if nd < pd then pd else nd) (tail xs) xs

inc :: [Int] -> [Int]
inc []       = [1]
inc (9 : xs) = 0 : inc xs
inc (x : xs) = (x + 1) : xs

goodPassword :: [Int] -> Bool
goodPassword ch = all (uncurry (>=)) ps && any (uncurry (==)) ps
  where ps = zip ch $ tail ch

goodPasswordStarTask :: [Int] -> Bool
goodPasswordStarTask ch = all (uncurry (>=)) (zip ch $ tail ch)
  && any ((== 2) . length) (groupBy (==) ch)

simple :: [Int] -> [Int] -> Int
simple ch1 ch2 =
  length $ filter goodPassword $ takeWhile (/= inc ch2) $ iterate inc ch1

simpleStarTask :: [Int] -> [Int] -> Int
simpleStarTask ch1 ch2 =
  length $ filter goodPasswordStarTask $ takeWhile (/= inc ch2) $ iterate inc
                                                                          ch1

main = do
  let
    [fromRaw, toRaw] = map digitToInt <$> splitOn "-" range
    from             = makeDownMono fromRaw
    to               = makeUpMono toRaw
    n                = length to
    dp               = passNums n
    (same, diff)     = span (uncurry (==)) $ zip from to
    lastSame         = if null same then -1 else fst (last same)
    downDiff         = map fst diff
    upDiff           = map snd diff
    hasDupl          = any (uncurry (==)) $ zip same $ tail same
    downHasDupl      = hasDupl || head downDiff == lastSame
    upHasDupl        = hasDupl || head upDiff == lastSame

    wholes           = sum
      [ firstDupl dp (hasDupl, d, length diff - 1)
      | d <- [head downDiff + 1 .. head upDiff - 1]
      ]

    countBetween genD (restNum, (hasDupl, count)) (d, pd) =
      ( restNum - 1
      , ( hasDupl || pd == d
        , count
--            + sum [ dp M.! (hasDupl || pd == i, i, restNum) | i <- genD d pd ]
          + sum
              [ firstDupl dp (hasDupl || pd == i, i, restNum - 1)
              | i <- genD d pd
              ]
        )
      )

    countFirst (b, d) = d + if b then 1 else 0

    ups =
      countFirst
        $ snd
        $ foldl (countBetween $ \d pd -> [pd .. d - 1])
                (length diff - 1, (upHasDupl, 0))
        $ zip (tail upDiff) upDiff

    downs =
      countFirst
        $ snd
        $ foldl (countBetween $ \d pd -> [d + 1 .. 9])
                (length diff - 1, (downHasDupl, 0))
        $ zip (tail downDiff) downDiff

  print $ simple (reverse from) (reverse to)
  print $ simpleStarTask (reverse from) (reverse to)
  print $ wholes + ups + downs
  print $ wholes
  print $ ups
  print $ downs
