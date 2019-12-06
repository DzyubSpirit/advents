fuel :: Int -> Int
fuel mas = max 0 $ div mas 3 - 2

fullFuel :: Int -> Int
fullFuel mas = sum $ takeWhile (> 0) $ drop 1 $ iterate fuel mas

main :: IO ()
main = do
  ms <- map read . lines <$> readFile "in"
  print $ sum $ map fuel ms
  print $ sum $ map fullFuel ms
