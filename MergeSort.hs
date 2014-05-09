-- re-work
mergesort' :: (Ord a) => [a] -> [a]
mergesort' []     = []
mergesort' xs@[x] = xs 
mergesort' xs     = merge' (mergesort' f) (mergesort' s)
   where (f, s)   = splitAt half xs
         half     = length xs `div` 2

merge' :: (Ord a) => [a] -> [a] -> [a]
merge' [] []                 = []
merge' [] ys                 = ys 
merge' xs []                 = xs 
merge' xxs@(x:xs) yys@(y:ys) = if x <= y then x : merge' xs yys else y : merge' xxs ys



-- [1,2,3,4] 
-- m (ms [1,2] ms [3,4]) 
-- m (ms[1]    ms[2]) 
-- m (ms[3]   ms[4])















-- bad. look at GroupImproved
group' :: (Ord a) => [a] -> [[a]]
group' [] = []
group' xs = takeWhile (== head' xs) (sorted xs) : (group' $ dropWhile (== head' xs) (sorted xs))
                    where sorted ys   = mergesort' (ys)
                          head' (x:_) = x
