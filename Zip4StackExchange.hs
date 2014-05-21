-- TODO: convert list of lists into tuple
zip4 :: [a] -> [a] -> [a] -> [a] -> [(a,a,a,a)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

--ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
--   [(2,2,5,2),(3,2,5,2),(3,2,3,2)] 