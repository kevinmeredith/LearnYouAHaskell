--Problem 4. A palindromic number reads the same both ways. The largest palindrome made
--from the product of two 2-digit numbers is 9009 = 91 × 99.
--Find the largest palindrome made from the product of two 3-digit numbers.

import Control.Applicative

isPalindrome :: Int => Bool
isPalindrome x = if (reverse . show) x == show x then True else False

findLargestPalindrome :: Int -> Maybe Int
findLargestPalindrome n = maybeLargest result
       where maybeLargest []    = Nothing
             maybeLargest ys = Just $ maximum ys
       	     result = filter (isPalindrome) products
       	     products = fs <*> xs
       	     fs = take n $ repeat (*) <*> xs
             xs = reverse $ [1..n]