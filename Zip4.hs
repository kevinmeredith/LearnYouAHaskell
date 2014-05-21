-- TODO: convert list of lists into tuple
zip4 :: [a] -> [a] -> [a] -> [a] -> [(a,a,a,a)]
zip4 xs ys zs as = zip4' xs ys zs as 
                    where zip4' [] _ _ _ = []
                          zip4' _ [] _ _ = []
                          zip4' _ _ [] _ = []
                          zip4' _ _ _ [] = []
                          zip4' (b:bs) (c:cs) (d:ds) (e:es) = (b,c,d,e) : zip4' bs cs ds es


--ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
--   [(2,2,5,2),(3,2,5,2),(3,2,3,2)] 