import Control.Monad (when)
import qualified Data.Array as A
import Data.Function (on)
import Data.List (groupBy)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  seatRows <- filter (/= "") . lines <$> readFile (head args)
  let width       = length $ head seatRows
      height      = length seatRows
      seatsArray  = A.listArray ((1, 1), (height, width)) $ concat seatRows
      seatsStates = iterate step seatsArray
  print $ length $ filter (== '#') $ A.elems $ fst $ head 
        $ dropWhile (uncurry (/=)) $ zip seatsStates $ tail seatsStates

type Seats = A.Array (Int, Int) Char 
  
step :: Seats -> Seats
step seats = A.array (A.bounds seats) $ map (seatStep seats) $ A.assocs seats

seatStep :: Seats -> ((Int, Int), Char) -> ((Int, Int), Char)
seatStep seats (ix, seat)
  | seat == '.'      = (ix, '.')
  | numOcc == 0 = (ix, '#')
  | numOcc >= 4 = (ix, 'L')
  | otherwise        = (ix, seat)
  where numOcc = numOccupied seats ix

numOccupied :: Seats -> (Int, Int) -> Int
numOccupied seats ix = length $ filter (== '#') 
                     $ map (findWithDefault '.' seats) $ neighbours ix

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (y, x) = [(y+dy, x+dx) | dx <- [-1..1], dy <-[-1..1]
                                  , dx /= 0 || dy /= 0]

findWithDefault :: Char -> Seats -> (Int, Int) -> Char
findWithDefault def arr ix
  | A.inRange (A.bounds arr) ix = arr A.! ix
  | otherwise = def

showSeats :: Seats -> String
showSeats = unlines . map (map snd) . groupBy ((==) `on` (fst . fst)) . A.assocs
