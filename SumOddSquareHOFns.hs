sumOddSq :: Int -> Int
sumOddSq x = length $ takeWhile (< x) $ map (^2) $ filter (odd) [1..]