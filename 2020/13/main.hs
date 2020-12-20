import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    fail $ "Expected one command line argument, got " ++ show args
  startStr:idsStr:_ <- lines <$> readFile (head args)
  let start = read startStr :: Int 
      ids = map read $ filter (/= "x") $ splitOn "," idsStr 
  print $ uncurry (*) $ head $ catMaybes
        $ zipWith (\time -> fmap $ (,) time) [0..] 
        $ map (`comingId` ids) [start..]

comingId :: Int -> [Int] -> Maybe Int
comingId time = listToMaybe . filter ((== 0) . (time `mod`))
