import Control.Monad (when)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy)
import System.Environment (getArgs)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (lift, minPrec, readPrec_to_P)

import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  exprs <- map read . lines <$> readFile (head args) :: IO [Expr]
  print $ sum $ map eval exprs

class Eval a where
  eval :: a -> Int

instance Eval Expr where
  eval (NumOrPar expr) = eval expr
  eval (Op opnd ops) = product . map (sum . map (eval . snd))
                     . groupBy (const $ (== '+') . fst) 
                     $ (undefined, opnd) : ops

instance Eval NumOrPar where
  eval (Number x) = x
  eval (Parentheses expr) = eval expr

data Expr = NumOrPar NumOrPar
          | Op NumOrPar [(Char, NumOrPar)]
  deriving (Show)

data NumOrPar = Number Int | Parentheses Expr
  deriving (Show)

instance Read Expr where
  readPrec = lift $ do
    P.skipSpaces
    opnd1 <- readP
    ops <- P.many $ P.skipSpaces >> opExpr
    return $ Op opnd1 ops
      where opExpr = do
              op <- P.satisfy (`elem` "+*")
              opnd <- P.skipSpaces >> readP
              return (op, opnd)

instance Read NumOrPar where
  readPrec = lift $ parExpr P.+++ numExpr
    where numExpr = Number . read <$> P.munch isDigit
          parExpr = do
            P.char '('
            expr <- Parentheses <$> readP
            P.skipSpaces >> P.char ')'
            return expr

readP :: Read a => P.ReadP a
readP = readPrec_to_P readPrec minPrec
