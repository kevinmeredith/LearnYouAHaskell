--transpose transposes a list of lists. If you look at a list of lists 
-- as a 2D matrix, the columns become the rows and vice versa.
transpose' :: [[a]] -> [[a]]
transpose' ys
   | null $ filter (not . null) ys = []
   | otherwise = [flatten' $ map head' ys] ++ transpose' (map tail' ys)

head' :: [a] -> [a]
head' []     = []
head' (x:xs) = [x]

tail' :: [a] -> [a]
tail' []     = []
tail' (x:xs) = xs

flatten' :: [[a]] -> [a]
flatten' as = foldl (\acc x -> acc ++ x)  [] as

-- transpose [[1,2,3],[4,5,6],[7,8,9]]  
-- [[1,4,7],[2,5,8],[3,6,9]]