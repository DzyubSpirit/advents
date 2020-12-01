import           Data.List.Split
import           Data.Array

data CompState = CompState
  { memory :: Array Int Int
  , pos :: Int
  , stdin :: [Int]
  , stdout ::[Int]
  } deriving (Show)

oneStep :: CompState -> CompState
oneStep cs@(CompState arr pos stdin stdout) = case mod opCode 100 of
  99 -> cs { pos = 1 + snd (bounds arr) }
  1  -> binOp (+)
  2  -> binOp (*)
  3  -> cs { memory = arr // [(op1Val, head stdin)]
           , pos    = pos + 2
           , stdin  = tail stdin
           }
  4 -> cs { pos = pos + 2, stdout = stdout ++ [op1] }
  5 -> if op1 /= 0 then cs { pos = op2 } else cs { pos = pos + 3 }
  6 -> if op1 == 0 then cs { pos = op2 } else cs { pos = pos + 3 }
  7 -> binOp (\a b -> if a < b then 1 else 0)
  8 -> binOp (\a b -> if a == b then 1 else 0)
 where
  opCode = arr ! pos
  readOp dig pos = if opCode `div` (10 ^ dig) `mod` 10 == 1
    then arr ! pos
    else arr ! (arr ! pos)

  op1Val = arr ! (pos + 1)
  op2Val = arr ! (pos + 2)
  op3Val = arr ! (pos + 3)
  op1    = readOp 2 (pos + 1)
  op2    = readOp 3 (pos + 2)

  binOp op = cs { memory = arr // [(op3Val, op op1 op2)], pos = pos + 4 }

initCompState pr stdin = CompState pr 0 stdin []

compute pr stdin =
  stdout
    $ head
    $ dropWhile ((<= snd (bounds pr)) . pos)
    $ iterate oneStep
    $ initCompState pr stdin

main = do
  pr <- map read . splitOn "," <$> readFile "star.in"
  let prM = listArray (0, length pr - 1) pr
  print $ compute prM [5]
