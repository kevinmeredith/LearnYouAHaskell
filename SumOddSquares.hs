sum' :: (Num a) => a -> a -> a
sum' x y = x + y                        

-- find the sum of all odd squares for [1..n] where
-- n is an argument
sumOddSquares :: Integer -> Integer
sumOddSquares n = let odd' x = (x `mod` 2) == 1
                      odds = filter odd' [1..n]
                      oddsSq = map (^2) odds
                      oddsSqSum = foldl sum' 0 oddsSq
                  in oddsSqSum

-- find the sum of all odd squares that are smaller than 10,000
sumAllOddSquaresLTTenThousand :: Integer
sumAllOddSquaresLTTenThousand = foldl sum' 0 sums
                        where sums = takeWhile (< 10000) (map sumOddSquares [1..])