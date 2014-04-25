-- implementing split again
split' :: (Ord a) => Ordering -> a -> [a] -> [a]
split' ord x ys = filter (\a -> compare a x == ord) ys


 -- 2 [1,2,3] LT === [1]