import Control.Monad (when)
import System.Environment (getArgs)

import qualified Data.Map.Strict as M

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  startNums <- map read . lines <$> readFile (head args)
  let initGame = Game
        { encounters = M.fromList [(x, [i]) | (x, i) <- zip startNums [0..]]
        , lastSpoken = last startNums
        , pos        = length startNums
        }
  print $ lastSpoken $ head $ dropWhile ((< 2020) . pos) 
        $ iterate speak initGame

type Number = Int
type Pos    = Int
data Game = Game
  { encounters :: M.Map Number [Pos]
  , lastSpoken :: Number
  , pos        :: Int
  } deriving (Show)

speak :: Game -> Game
speak game@(Game enc spoken pos) =
  case enc M.! spoken of
    [x]    -> updGame 0
    [x, y] -> updGame (x - y) 
    _      -> error "unexpected that the number isn't in the game"
  where updGame num = Game
          { encounters = M.alter (Just . maybe [pos] (take 2 . (pos:))) num enc
          , lastSpoken = num
          , pos        = pos + 1
          }
