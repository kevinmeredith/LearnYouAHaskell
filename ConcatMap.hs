-- concatMap is the same as first mapping a function to a list 
-- and then concatenating the list with concat.
concatMap' :: (a -> a) -> [a] -> [a]
concatMap'


--ghci> concatMap (replicate 4) [1..3]  
--[1,1,1,1,2,2,2,2,3,3,3,3]  