split' :: (Ord a) => [a] -> a -> Ordering -> [a]
split' [] _ _ = []
split' (x:xs) a LT 
  | x < a     = x : split' xs a LT
  | otherwise = split' xs a LT
split' (x:xs) a GT 
  | x >= a     = x : split' xs a GT
  | otherwise = split' xs a GT

--quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' xs 
	| sorted' ys == True = xs
	| otherwise = quicksort' xs
	where ys = split' xs (head xs) LT ++ split' xs (head xs) GT

sorted' :: (Ord a) => [a] -> Bool
sorted' []     = True
sorted' [x]    = True
sorted' (x:xs) = if x > (head xs) then False else sorted' xs
