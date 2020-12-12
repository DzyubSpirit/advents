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
      possibleToggles = [ line | (line, instruct) <- A.assocs instructsArr, toggleable instruct]
      fixedInstructs = map (\line -> modify toggle line instructsArr) possibleToggles      
  maybe (print "No good fix is possible") (print . accumulator . snd) $ safeHead $ dropWhile (not . uncurry finished) [(inst, run inst) | inst <- fixedInstructs]

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing
  
modify :: A.Ix i => (a -> a) -> i -> A.Array i a -> A.Array i a
modify f i arr = arr A.// [(i, f $ arr A.! i)]
  
data MachineState = MachineState
  { line :: Int
  , accumulator :: Int
  , visited :: S.Set Int
  } deriving (Show)

initial :: MachineState
initial = MachineState 0 0 S.empty

run :: A.Array Int Instruction -> MachineState
run instructs = head 
        $ dropWhile (\st -> not (looped st) && not (finished instructs st)) 
        $ iterate (step instructs) initial 

looped :: MachineState -> Bool
looped st = S.member (line st) (visited st)

finished :: A.Array Int Instruction -> MachineState -> Bool
finished instructs st = not $ A.inRange (A.bounds instructs) (line st)

data Instruction = Acc Int | Jmp Int | Nop Int
  deriving (Show)

toggleable :: Instruction -> Bool
toggleable (Acc _) = False
toggleable _ = True

toggle :: Instruction -> Instruction
toggle (Jmp x) = Nop x
toggle (Nop x) = Jmp x
toggle x       = error "non toggleable"

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
