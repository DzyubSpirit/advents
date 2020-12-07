import Control.Monad (when)
import Data.Char (isSpace, isDigit)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import Text.Read (readPrec, readMaybe)
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadPrec (lift, ReadPrec)

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

class Valid a where
  isValid :: a -> Bool

instance Valid Passport where
  isValid (Passport passport) =
    S.size (S.intersection fields required) == S.size required && 
    S.size (S.intersection fields optional) == numOptionalFields &&
    all isFieldValid (M.assocs passport)
      where fields = S.fromList $ M.keys passport
            numOptionalFields = S.size fields - S.size required 

isFieldValid :: (String, String) -> Bool
isFieldValid ("byr", str) = maybe False isValid (readMaybe str :: Maybe BirthYear)
isFieldValid ("iyr", str) = maybe False isValid (readMaybe str :: Maybe IssueYear)
isFieldValid ("eyr", str) = maybe False isValid (readMaybe str :: Maybe ExpirationYear)
isFieldValid ("hgt", str) = maybe False isValid (readMaybe str :: Maybe Height)
isFieldValid ("hcl", str) = isJust (readMaybe str :: Maybe HairColor)
isFieldValid ("ecl", str) = isJust (readMaybe str :: Maybe EyeColor)
isFieldValid ("pid", str) = isJust (readMaybe str :: Maybe PassportID)
isFieldValid _            = True

newtype BirthYear = BirthYear Int

instance Read BirthYear where
  readPrec = fmap BirthYear fourDigits

instance Valid BirthYear where
  isValid (BirthYear year) = 1920 <= year && year <= 2002

newtype IssueYear = IssueYear Int

instance Read IssueYear where
  readPrec = fmap IssueYear fourDigits

instance Valid IssueYear where
  isValid (IssueYear year) = 2010 <= year && year <= 2020

newtype ExpirationYear = ExpirationYear Int

instance Read ExpirationYear where
  readPrec = fmap ExpirationYear fourDigits

instance Valid ExpirationYear where
  isValid (ExpirationYear year) = 2020 <= year && year <= 2030

fourDigits :: ReadPrec Int
fourDigits = nDigits 4

nDigits :: Int -> ReadPrec Int
nDigits n = lift $ fmap read
          $ P.count n (P.satisfy isDigit) <* P.eof

data Height = Centimetres Int | Inches Int
  deriving (Show)

instance Read Height where
  readPrec = lift $ do
    num <- read <$> P.munch isDigit
    units <- P.string "cm" P.+++ P.string "in"
    return $ case units of
      "cm" -> Centimetres num
      "in" -> Inches num

instance Valid Height where
  isValid (Centimetres cms) = 150 <= cms && cms <= 193
  isValid (Inches inches) = 59 <= inches && inches <= 76

data HairColor = HairColor
  deriving (Show)

instance Read HairColor where
  readPrec = lift $ P.char '#' >> P.count 6 validChar >> return HairColor
    where validChar = P.satisfy $ \c -> isDigit c || c `elem` ['a','b'..'f']

data EyeColor = EyeColor
  deriving (Show)

instance Read EyeColor where
  readPrec = lift $ do
    foldl1 (P.+++)
         $ map P.string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    return EyeColor

data PassportID = PassportID
  deriving (Show)

instance Read PassportID where
  readPrec =  nDigits 9 >> return PassportID 
