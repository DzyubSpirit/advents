import Control.Monad (when)
import System.Environment (getArgs)

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (lift)

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  instructs <- map read .filter (not . null) . splitOn "\n" 
              <$> readFile (head args) :: IO [Instruction]
  let instructsArr = A.listArray (0, length instructs - 1) instructs
  print $ accumulator $ head 
        $ dropWhile (\st -> S.notMember (line st) (visited st)) 
        $ iterate (step instructsArr) initial 
  
data MachineState = MachineState
  { line :: Int
  , accumulator :: Int
  , visited :: S.Set Int
  } deriving (Show)

initial :: MachineState
initial = MachineState 0 0 S.empty

data Instruction = Acc Int | Jmp Int | Nop Int
  deriving (Show)

step :: A.Array Int Instruction -> MachineState -> MachineState
step instructs st@(MachineState line acc visited) =
  case instructs A.! line of
    Acc add     -> st' { line = line + 1, accumulator = acc + add } 
    Jmp offset -> st' { line = line + offset }
    Nop _       -> st' { line = line + 1 }
    where st' = st { visited = S.insert line visited }

instance Read Instruction where
  readPrec = lift $ foldl1 (P.+++) [
      fmap Acc $ P.string "acc " >> number,
      fmap Jmp $ P.string "jmp " >> number,
      fmap Nop $ P.string "nop " >> number
    ]

number :: P.ReadP Int
number = neg P.+++ pos
  where neg = fmap (negate . read) $ P.char '-' >> P.munch isDigit
        pos = fmap read $ P.char '+' >> P.munch isDigit

  
