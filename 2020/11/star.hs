import Control.Monad (when)
import qualified Data.Array as A
import Data.Bifunctor (bimap)
import Data.Maybe (listToMaybe)
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
  | numOcc >= 5 = (ix, 'L')
  | otherwise        = (ix, seat)
  where numOcc = numOccupied seats ix

numOccupied :: Seats -> (Int, Int) -> Int
numOccupied seats ix = sum $ map 
                     ( maybe 0 (fromEnum . (== '#')) . listToMaybe
                     . dropWhile (== '.')
                     . map (seats A.!)
                     . takeWhile (A.inRange $ A.bounds seats) 
                     . tail . (\(dy, dx) -> iterate (bimap (+ dy) (+ dx)) ix)
                     ) directions

directions :: [(Int, Int)]
directions = [(dy, dx) | dx <- [-1..1], dy <-[-1..1]
                       , dx /= 0 || dy /= 0]

findWithDefault :: Char -> Seats -> (Int, Int) -> Char
findWithDefault def arr ix
  | A.inRange (A.bounds arr) ix = arr A.! ix
  | otherwise = def

showSeats :: Seats -> String
showSeats = unlines . map (map snd) . groupBy ((==) `on` (fst . fst)) . A.assocs
