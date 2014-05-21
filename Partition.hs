partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f [] = ([], [])
partition'  f (x:xs)
   | f x       = (x:as, bs)
   | otherwise = (as  , x:bs)
   where (as, bs) = partition' f xs

-- ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
-- ("BOBMORGAN","sidneyeddy")  
-- ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]  
-- ([5,6,7],[1,3,3,2,1,0,3])  