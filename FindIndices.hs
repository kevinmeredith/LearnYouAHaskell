findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' _ [] = []
findIndices' f ys = findIndices'' f ys 0
                    where findIndices'' _ [] _ = []
                    	  findIndices'' g (x:xs) i
                            | g x       = i : findIndices'' g xs (i+1)
                            | otherwise = findIndices'' g xs (i+1)
