import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.Bits (xor)
import Data.Char (isDigit)
import Data.List ((!!))
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
  { positions :: (Int, Int)
  , symb :: Char
  , password :: String
  } deriving (Show)

instance Read Password where
  readPrec = lift $ do
    pos1 <- read <$> munch isDigit
    char '-'
    pos2 <- read <$> munch isDigit
    char ' '
    ch <- get
    string ": "
    pass <- munch (const True)
    return $ Password (pos1, pos2) ch pass

isValid :: Password -> Bool
isValid (Password (pos1, pos2) ch pass) =
  validCharAtPos (pos1-1) `xor` validCharAtPos (pos2-1)
  where validCharAtPos pos = pass !! pos == ch
