--breaks it when the predicate is first true
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ [] = ([], [])
break' f ys = break'' f ys []
               where break'' _ [] acc = (reverse acc, [])
               	     break'' g xxs@(x:xs) acc 
               	        | g x       = (reverse acc, xxs)
               	        | otherwise = break'' g xs (x:acc)

-- ghci> break (==4) [1,2,3,4,5,6,7]  
-- ([1,2,3],[4,5,6,7])  