import Control.Monad (when)
import System.Environment (getArgs)

import Data.Char (isDigit)
import Text.Read (readPrec, readMaybe)
import Text.ParserCombinators.ReadPrec (lift)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  rules <- map (read :: String -> Rule) . lines <$> readFile (head args)
  let tree = reverseRules rules
  print $ (+ (-1)) $ S.size $ bfs tree S.empty ["shiny gold"]

type Color = String
data Rule = Rule 
  { color :: Color
  , children :: M.Map Color Int
  } deriving (Show)

instance Read Rule where
  readPrec = lift $ do
    color <- stringTill " bags contain "
    let childrenP = do
          res <- M.fromList <$> P.sepBy child (P.string ", ") 
          P.char '.'
          P.eof
          return res
        noChildrenP = P.string "no other bags." >> return M.empty
    children <- childrenP P.<++ noChildrenP
    return $ Rule color children

child :: P.ReadP (Color, Int)
child = do
    countM <- readMaybe <$> P.munch (isDigit)
    count <- maybe P.pfail return countM
    P.char ' '
    color <- stringTill " bag"
    P.optional $ P.char 's'
    return (color, count)
    
stringTill :: String -> P.ReadP String
stringTill end = P.manyTill (P.satisfy $ const True) $ P.string end

reverseRules :: [Rule] -> M.Map Color (S.Set Color)
reverseRules rules = foldr (\(k, v) -> M.alter (Just . maybe (S.singleton v) (S.insert v)) k) M.empty $ do
  rule <- rules
  child <- M.keys $ children rule
  return (child, color rule)

bfs :: M.Map Color (S.Set Color) -> S.Set Color -> [Color] -> S.Set Color
bfs tree visited [] = visited
bfs tree visited (x:xs) = bfs tree (S.insert x visited) $ xs ++ add
  where add | S.member x visited = []
            | otherwise          = maybe [] S.toList (M.lookup x tree)
