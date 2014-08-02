--Problem 3. The prime factors of 13195 are 5, 7, 13 and 29. What is the
--largest prime factor of the number 600851475143?

-- Note that sqrt n is the superior algorithm to this slow one
-- Its name is "Sieve ..." (it was mentioned in Math class at Nova)

prime :: Integer -> Bool
prime x = prime' x (x `div` 2) where
	      prime' _ 1 = True
	      prime' x y = if x `mod` y == 0 then False
	      	           else prime' x (y - 1)

largestPrimeFactor :: Integer => Maybe Integer
largestPrimeFactor x = largestPrimeFactor' x (x `div` 2)	      	           
             where largestPrimeFactor' _ 1 = Nothing
                   largestPrimeFactor' x y = if x `mod` y == 0 && prime y then Just y
	      	                                 else largestPrimeFactor' x (y - 1)