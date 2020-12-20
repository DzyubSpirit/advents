import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Char (isDigit)
import Numeric (readInt)
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

  let initMemory = Memory M.empty (Mask 0 0)
  print $ sum $ M.elems $ dat $ foldl execStmt initMemory stmts

data Statement = UpdateMaskStatement Mask | AssignmentStatement Assignment

instance Read Statement where
  readPrec = (UpdateMaskStatement <$> readPrec) +++ 
             (AssignmentStatement <$> readPrec)


data Mask = Mask 
  { maskOnInput  :: Int
  , replacedBits :: Int
  } deriving (Show)
  
instance Read Mask where
  readPrec = lift $ do
    P.string "mask = "
    maskStr <- P.munch (`elem` "01X")
    maskOnInput  <- P.readS_to_P $ const 
      $ readInt 2 (`elem` "01X") (fromEnum . (== 'X')) maskStr
    replacedBits <- P.readS_to_P $ const 
      $ readInt 2 (`elem` "01X") (fromEnum . (== '1')) maskStr
    return $ Mask maskOnInput replacedBits

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
execStmt mem@(Memory dat mask) (AssignmentStatement ass) = 
  mem { dat = updDat dat }
  where updDat = M.insert (addr ass) ((val ass) .&. maskOnInput + replacedBits) 
        Mask maskOnInput replacedBits = mask
