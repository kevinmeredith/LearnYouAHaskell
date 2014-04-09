multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x*y*z

-- partial function
--*Main> let x = multThree 1 2
--*Main> x 3
--6