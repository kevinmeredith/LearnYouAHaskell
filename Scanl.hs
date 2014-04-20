--report intermediate values of the fold steps
scanl' :: (a -> b -> b) -> b -> [a] -> [b]
scanl' _ acc []       = [acc]
scanl' f acc (y:ys) = acc : (scanl' f (f y acc) ys)

-- scanl' (+) 0 [1,2,3]
-- 0 + scanl' (+) 

-- foldl (+) 0 [1,2,3]
-- 0 1 3 6