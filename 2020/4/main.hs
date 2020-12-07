import Control.Monad (when)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Text.Read (readPrec)
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadPrec (lift)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  passports <- readPassports <$> readFile (head args)
  print $ length $ filter isValid passports

newtype Passport = Passport (M.Map String String)
  deriving (Show)

readPassports :: String -> [Passport]
readPassports = map read . splitOn "\n\n"

instance Read Passport where
  readPrec = lift $ fmap (Passport  . M.fromList)
           $ P.many (field <* P.skipSpaces) <* P.eof
    where field = do
            name <- P.munch (/= ':')
            P.char ':'
            val  <- P.munch (not . isSpace)
            return (name, val)

required, optional :: S.Set String
required = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
optional = S.fromList ["cid"]

isValid :: Passport -> Bool
isValid (Passport passport) =
  S.size (S.intersection fields required) == S.size required && 
  S.size (S.intersection fields optional) == S.size fields - S.size required
    where fields = S.fromList $ M.keys passport
