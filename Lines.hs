lines' :: String -> [String]
lines' [] = []
lines' "\n" = [""]
lines' xs = lines'' xs []
             where lines'' [] line = reverse $ line : []
             	   lines'' (x:xs) line 
                     |  x == '\n' = line : lines'' xs []
                     | otherwise  = lines'' xs (x : line)

--lines "first line\nsecond line\nthird line"  
