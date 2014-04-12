sum' :: (Num a) => a -> a -> a
sum' x y = x + y                   

odd' :: (Integral a) => a -> Bool
odd' x = mod x 2 == 1     

-- find the sum of all odd squares that are smaller than 10,000
sumAllOddSquaresLTTenThousand :: Integer
sumAllOddSquaresLTTenThousand = foldl sum' 0 lessThan10k
                        where odds = filter odd' [1..]
                              squared = map (^2) odds 
                              lessThan10k = takeWhile (< 10000) squared


