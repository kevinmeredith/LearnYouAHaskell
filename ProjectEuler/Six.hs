  --Problem 6. The sum of the squares of the first ten natural numbers is:
  --   1^2 + 2^2 + ... + 10^2 = 385
  --The sum of the square of the first ten natural numbers is:
  --   (1 + 2 + ... + 10)^2 = 55^2 = 3025
  --Hence the difference is 2640.
  --Find the above for the first 100 numbers.

sumOfSqs :: Integer -> Integer
sumOfSqs n = sumOfSqs' [1..n]
sumOfSqs'  = foldr ((+) . (^2)) 0

sumOfSq :: Integer -> Integer
sumOfSq n = sumOfSq' [1..n] 
 where sumOfSq' xs = (^2) $ foldr (+) 0 xs 