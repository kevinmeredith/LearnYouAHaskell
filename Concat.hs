--concat flattens a list of lists into just a list of elements
concat' :: [[a]] -> [a]
concat' ys = foldl (\acc x -> acc ++ x) [] ys

--ghci> concat ["foo","bar","car"]  
--"foobarcar"  
--ghci> concat [[3,4,5],[2,3,4],[2,1,1]]  
--[3,4,5,2,3,4,2,1,1]  

-- concatMap is the same as first mapping a function to a list 
-- and then concatenating the list with concat.
concatMap' :: (a -> [a]) -> [a] -> [a]
concatMap' f xs = concat' $ map f xs

--ghci> concatMap (replicate 4) [1..3]  
--[1,1,1,1,2,2,2,2,3,3,3,3]  