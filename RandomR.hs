-- returns a random number inclusively between two values
-- TODO: fix! can't use `mod` now due to `Random a` type.
import System.Random

randomR' :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
randomR' (min, max) g = let (num, gen) = random g
                        in (num `mod` min + min, gen)

-- back of the "envelope" calculationss
-- (5, 10)
-- 55 / 5 = 0 + 5
-- 59 / 5 = 4 + 5
-- 62 / 5 = 3 + 5 
