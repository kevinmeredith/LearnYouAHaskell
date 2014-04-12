-- largest divisible LYAH
-- notice that, once a number is found is divisible by 3829, 
-- return it
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = (x `mod` 3829) == 0