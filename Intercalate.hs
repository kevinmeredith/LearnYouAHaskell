-- intercalate takes a list of lists and a list. 
-- It then inserts that list in between all those lists and then flattens the result.

intercalate' :: [[a]] -> [a] -> [a] 
intercalate' [[]] _ = []
intercalate' xxs ys =  intercalate'' xxs ys 
                     where intercalate'' [[]] _ = []
                           intercalate'' (a:as) i 
                              | null as = a
                              | otherwise = a ++ i ++ (intercalate'' as i)
