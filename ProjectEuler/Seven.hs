--Problem 7. By listing the first six prime numbers, we can see that
--the 6th prime is 13.
--What is the 10,001st prime number?

-- TODO: replace with sqrt strategy!
prime :: Integer -> Bool
prime x = prime' x (x `div` 2) where
	      prime' _ 1 = True
	      prime' x y = if x `mod` y == 0 then False
	      	           else prime' x (y - 1)

getNthPrime :: Int -> Integer
getNthPrime n = head $ drop (n-1) $ filter (prime) [2..]