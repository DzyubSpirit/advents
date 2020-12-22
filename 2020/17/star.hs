import Control.Monad (when)
import System.Environment (getArgs)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  petriDish <- lines <$> readFile (head args)
  let initGame = S.fromList [ (x, y, 0, 0)
                            | (y, petriLine) <- zip [1..] petriDish
                            , (x,      cell) <- zip [1..] petriLine
                            , cell == '#'
                            ]
  print $ numActive $ head $ drop 6 $ iterate conwayCycle initGame
  
type Game = S.Set Coord
type Coord = (Int, Int, Int, Int)

numActive :: Game -> Int
numActive = S.size

conwayCycle :: Game -> Game
conwayCycle game = M.keysSet . M.filterWithKey stillAlive
                 . foldr (M.alter $ Just . maybe 1 (+ 1)) M.empty 
                 . concatMap neighbours $ S.elems game
  where stillAlive coord 3 = True
        stillAlive coord 2 = S.member coord game
        stillAlive _     _ = False


neighbours :: Coord -> [Coord]
neighbours (x, y, z, w) = [ (x+dx, y+dy, z+dz, w+dw)
                          | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1]
                          , dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0
                          ]
