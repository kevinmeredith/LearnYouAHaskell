--LYAH
import System.Random

--We could make a function that generates a finite stream of numbers and a new generator like this:
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)  
finiteRandoms n g 
  | n == 0 = (num : randoms' gen, gen)
  | otherwise = (num: randoms')


randoms' g = num : randoms' gen
   where (num, gen) = random g 