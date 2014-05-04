splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n ys 
      | n < 0     = ([], ys)
      | otherwise = splitAt'' n ys []
      where splitAt'' a (x:xs) acc
                  | a == 0 = (acc, x:xs)
                  | null xs = (acc ++ [x], [])
                  | otherwise = splitAt'' (a-1) xs (acc ++ [x])