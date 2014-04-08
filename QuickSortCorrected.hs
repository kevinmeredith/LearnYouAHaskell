split' :: (Ord a) => [a] -> a -> Ordering -> [a]
split' [] _ _ = []
split' (x:xs) a LT
  | x < a     = x : split' xs a LT
  | otherwise = split' xs a LT 
split' (x:xs) a GT
  | x > a     = x : split' xs a GT
  | otherwise = split' xs a GT
-- non-exhaustive, unacceptable

quicksort :: (Ord a) => [a] => [a]
quicksort [] = []
quicksort (x:xs) 
   | isSortedd rs = rs
   | otherwise = quicksort left ++ [x] ++ quicksort right
     where left = split' xs x LT
           right = split' xs x GT
           headCount = count' (x:xs) x
           rs = left ++ (replicate headCount x) ++ right

count' :: (Eq a) => [a] -> a -> Int
count' [] _ = 0
count' (x:xs) y 
  | x == y = 1 + count' xs y
  | otherwise = count' xs y

-- for the [a], does an item exists that satisfies the predicate?
find' :: [a] -> (a -> Bool) -> Bool
find' [] _ = False
find' (x:xs) f 
  | f x = True
  | otherwise = find' xs f 

gt' :: (Ord a) => a -> a -> Bool
gt' a b 
 | a > b     = True
 | otherwise = False

isSortedd :: (Ord a) => [a] -> Bool
isSortedd [] = True
isSortedd (x:xs) 
  | find' rs (== True) = False -- when applying gt' to each item, we found a "True"
  | otherwise = isSortedd xs
  where rs = map (gt' x) xs