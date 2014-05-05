-- re-work
mergesort' :: (Ord a) => [a] -> [a]
mergesort' []     = []
mergesort' xs = merge' (sort' fst', sort' snd')
   where fst' = take half xs
         snd' = drop half xs
         half = len `div` 2 
         len = length xs

merge' :: (Ord a) => ([a], [a]) -> [a]
merge' ([], [])      = []
merge' ([], ys)      = ys
merge' (xs, [])      = xs 
merge' (x:xs, y:ys) = if x < y then x : merge' (xs, y:ys) else y : merge'(x:xs, ys)

sort' :: (Ord a) => [a] -> [a]
sort' []     = []
sort' (x:xs) = m : sort' rest
  where m = foldl (\acc x -> if x < acc then x else acc) x xs
        rest = filterOne' (/= m) (x:xs)

filterOne' :: (a -> Bool) -> [a] -> [a]
filterOne' _ []     = []
filterOne' f (x:xs) = if not $ f x then xs else x : filterOne' f xs