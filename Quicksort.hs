split' :: (Ord a) => [a] -> a -> Ordering -> [a]
split' [] _ _ = []
split' (x:xs) a LT
  | x < a     = x : split' xs a LT
  | otherwise = split' xs a LT 
split' (x:xs) a GT
  | x > a     = x : split' xs a GT
  | otherwise = split' xs a GT

quicksort :: (Ord a) => [a] => [a]
quicksort [] = []
quicksort (x:xs) 
   | isSorted' rs rs (head rs) = rs
   | otherwise = quicksort left ++ [x] ++ quicksort right
     where left = split' xs x LT
           right = split' xs x GT
           rs = left ++ [x] ++ right

isSorted' :: (Ord a) => [a] -> [a] -> a -> Bool
isSorted' [] [] _ = True
isSorted' (x:xs) [] _ = isSorted' xs xs (head xs)
isSorted' orig (x:xs) a
  | a > x = False
  | otherwise = isSorted' orig xs a