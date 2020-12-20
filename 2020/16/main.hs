import Control.Monad (when)
import Data.Char (isDigit)
import Data.List ((!!), (\\), intersect)
import Data.Ix (range)
import System.Environment (getArgs)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (lift, minPrec, readPrec_to_P, ReadPrec)

import qualified Data.Map.Strict as M
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  InputFile fields ticket nearbyTickets <- read <$> readFile (head args)
  let validNumbers = foldr (\(fieldIx, num) -> 
                              M.alter (Just . maybe [fieldIx] (fieldIx:))
                                      num
                           ) 
                           M.empty
                           [ (ix, num) 
                           | (ix, field) <- zip [0..] fields
                           ,         num <- concatMap range $ intervals field
                           ] 
      validTickets = filter (all (`M.member` validNumbers) . nums) 
                            nearbyTickets
      positions = [ map ((!! pos) . nums) validTickets 
                  | pos <- [0..length fields - 1]]
      posFields = map (foldl1 intersect . map (validNumbers M.!)) positions
      step (posFields, assMap) = ( map (\\ (map snd nonAmbPoses)) posFields
                                 , M.union assMap $ M.fromList nonAmbPoses)
        where nonAmbPoses = nonAmbiguousPos posFields
      posField = snd $ head 
        $ dropWhile ((< length fields). M.size . snd) 
        $ iterate step (posFields, M.empty)
  print $ product $ map ((nums ticket !!) . fst) 
        $ filter (hasPrefix "departure" . snd) 
        $ map (fmap $ name . (fields !!)) $ M.assocs posField

hasPrefix :: String -> String -> Bool
hasPrefix prefix str
  | length str < length prefix = False
  | otherwise                  = and $ zipWith (==) prefix str

type Pos     = Int
type FieldIx = Int

nonAmbiguousPos :: [[FieldIx]] -> [(Pos, FieldIx)]
nonAmbiguousPos positions = [ (pos, head fields) 
                            | (pos, fields) <- zip [0..] positions
                            ,  length fields == 1
                            ]

data InputFile = InputFile 
  { fields :: [Field]
  , ticket :: Ticket
  , nearbyTickets :: [Ticket]
  } deriving (Show)

instance Read InputFile where
  readPrec = lift $ do
    fields <- P.sepBy1 readP (P.char '\n')
    P.skipSpaces >> P.string "your ticket:\n"
    ticket <- readP
    P.skipSpaces >> P.string "nearby tickets:\n"
    nearbyTickets <- P.sepBy1 readP (P.char '\n')
    return $ InputFile fields ticket nearbyTickets

data Field = Field
  { name      :: String
  , intervals :: [(Int, Int)]
  } deriving (Show)

instance Read Field where
  readPrec = lift $ do
    name <- P.munch (/= ':')
    P.char ':' >> P.skipSpaces
    intervals <- P.sepBy1 interval (P.string " or ")
    return $ Field name intervals

interval :: P.ReadP (Int, Int)
interval = do
  low <- read <$> P.munch isDigit
  P.char '-'
  high <- read <$> P.munch isDigit
  return (low, high)

data Ticket = Ticket { nums :: [Int] }
  deriving (Show)

instance Read Ticket where
  readPrec = lift $ fmap Ticket $ P.sepBy1 readP (P.char ',')

readP :: Read a => P.ReadP a
readP = readPrec_to_P readPrec minPrec
