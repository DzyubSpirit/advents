import Control.Monad (when)
import Data.Function (on)
import Data.List (uncons)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  instructs <- map (fmap read . fromJust . uncons) . lines 
            <$> readFile (head args) :: IO [Instruct]
  print $ uncurry ((+) `on` abs) $ coords 
        $ foldl (flip doInstruct) (Ship (0,0) (10,1)) instructs 
  
type Instruct = (Char, Int)
data Ship = Ship 
  { coords    :: (Int, Int) 
  , direction :: (Int, Int)
  } deriving (Show)

doInstruct :: Instruct -> Ship -> Ship
doInstruct ('F', dist) (Ship (x, y) dir@(dx, dy)) =
  Ship (x + dx*dist, y + dy*dist) dir
doInstruct ('L', degree) ship@(Ship _ (dx, dy)) =
  case degree `mod` 360 of
    0   -> ship
    90  -> ship { direction = (-dy,  dx) }
    180 -> ship { direction = (-dx, -dy) }
    270 -> ship { direction = ( dy, -dx) }
    _   -> error $ "unknown angle: " ++ show degree
doInstruct ('R', degree) ship = doInstruct ('L', -degree) ship
doInstruct (dirChar, dist) ship@(Ship _ (dx, dy)) =
  ship { direction = (dx + ddx*dist, dy + ddy*dist) }
    where (ddx, ddy) = dirCharToVec dirChar

dirCharToVec :: Char -> (Int, Int)
dirCharToVec 'N' = ( 0,  1)
dirCharToVec 'S' = ( 0, -1)
dirCharToVec 'E' = ( 1,  0)
dirCharToVec 'W' = (-1,  0)
dirCharToVec dir = error $ "uknown direction: " ++ show dir
