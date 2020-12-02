import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import System.Environment (getArgs)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadP (char, get, munch, string)
import Text.ParserCombinators.ReadPrec (lift)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  passwords <- map read . lines <$> readFile (head args) :: IO [Password]
  print $ length $ filter isValid passwords

data Password = Password
  { numSymbRange :: (Int, Int)
  , symb :: Char
  , password :: String
  } deriving (Show)

instance Read Password where
  readPrec = lift $ do
    low <- read <$> munch isDigit
    char '-'
    high <- read <$> munch isDigit
    char ' '
    ch <- get
    string ": "
    pass <- munch (const True)
    return $ Password (low, high) ch pass

isValid :: Password -> Bool
isValid (Password (minNum, maxNum) ch pass) = numCh >= minNum && numCh <= maxNum
  where numCh = length $ filter (== ch) pass
