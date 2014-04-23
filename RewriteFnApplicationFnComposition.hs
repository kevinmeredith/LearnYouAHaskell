-- write the following with fn composition or fn application
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
foo' :: (Num a, Ord a) => [a]
foo' = replicate 100 $ product $ map (*3) $ zipWith max [1,2,3,4,5] [4,5,6,7,8] 