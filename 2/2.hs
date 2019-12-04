import           Data.List.Split
import           Data.Array

oneStep :: (Array Int Int, Int) -> (Array Int Int, Int)
oneStep (arr, i) = case arr ! i of
  99 -> (arr, snd $ bounds arr)
  1  -> ins (+)
  2  -> ins (*)
 where
  op1 = arr ! (arr ! (i + 1))
  op2 = arr ! (arr ! (i + 2))
  res = arr ! (i + 3)
  ins op = (arr // [(res, op op1 op2)], i + 4)

compute pr arg1 arg2 =
  (! 0) $ fst $ head $ dropWhile ((< (snd $ bounds pr)) . snd) $ iterate
    oneStep
    (pr // [(1, arg1), (2, arg2)], 0)

main = do
  pr <- map read . splitOn "," <$> readFile "2.in"
  let prM = listArray (0, length pr - 1) pr
  print $ compute prM 12 2
  print $ head
    [ arg1 * 100 + arg2
    | arg1 <- [0 .. 99]
    , arg2 <- [0 .. 99]
    , compute prM arg1 arg2 == 19690720
    ]
