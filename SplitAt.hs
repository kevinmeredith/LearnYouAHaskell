splitAt' :: Int -> [a] -> [[a]]
splitAt' n ys 
  | n < 0     = [[], ys]
  | otherwise = splitAt'' n ys []
          where splitAt'' a acc 
            | a == 0    = [(x:acc), xs]
            | null xs   = [acc, []]
            | otherwise = splitAt'' (a-1) xs (x:acc)