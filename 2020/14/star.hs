import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Char (digitToInt, isDigit, intToDigit)
import Numeric (readInt, showIntAtBase)
import System.Environment (getArgs)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (lift, (+++))

import qualified Data.Map.Strict as M
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  stmts <- map read . lines <$> readFile (head args)

  let initMemory = Memory M.empty (Mask $ replicate 36 '0')
  print $ sum $ M.elems $ dat $ foldl execStmt initMemory stmts

data Statement = UpdateMaskStatement Mask | AssignmentStatement Assignment

instance Read Statement where
  readPrec = (UpdateMaskStatement <$> readPrec) +++ 
             (AssignmentStatement <$> readPrec)


data Mask = Mask String deriving (Show)
  
instance Read Mask where
  readPrec = lift $ do
    P.string "mask = "
    Mask <$> P.munch (`elem` "01X")

data Assignment = Assignment
  { addr :: Int
  , val  :: Int
  } deriving (Show)

instance Read Assignment where
  readPrec = lift $ do
    P.string "mem["
    addr <- read <$> P.munch isDigit
    P.string "] = "
    val  <- read <$> P.munch isDigit
    return $ Assignment addr val

data Memory = Memory 
  { dat  :: M.Map Int Int
  , mask :: Mask
  } deriving (Show)

execStmt :: Memory -> Statement -> Memory
execStmt mem (UpdateMaskStatement mask) = mem { mask = mask }
execStmt mem@(Memory dat (Mask mask)) (AssignmentStatement ass) = 
  mem { dat = foldr (`M.insert` (val ass)) dat 
            $ map (fst . head . readInt 2 (const True) digitToInt) 
            $ mirrorAddrs maskedAddr }
    where
        addrStr    = showIntAtBase 2 intToDigit (addr ass) ""
        addrStr'   = replicate (36 - length addrStr) '0' ++ addrStr
        maskedAddr = zipWith merge mask addrStr'
        merge '0' x = x
        merge  x  _ = x

mirrorAddrs :: String -> [String]
mirrorAddrs       [] = [[]]
mirrorAddrs ('X':xs) = map ('0':) addrs ++ map ('1':) addrs
  where addrs = mirrorAddrs xs
mirrorAddrs ( x :xs) = map (x:) $ mirrorAddrs xs
