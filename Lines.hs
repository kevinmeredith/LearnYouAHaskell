lines' :: String -> [String]
lines' [] = []
lines' xs = lines'' xs []
             where lines'' [] ys = ys : []
             	   lines'' (x:xs) ys 
                     |  x == '\n' = ys : lines'' xs []
                     | otherwise  = lines'' xs (ys ++ [x])

--lines "first line\nsecond line\nthird line"  
