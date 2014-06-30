import System.Random

-- We could make a function that generates a finite stream of numbers and a new generator.
-- TODO: convert `Int` to `Num` and figure out how to call `take` on n
-- Note that FiniteRandomsLYAH.hs properly returns the final gen. 
-- In this version, it incorrectly returns the next gen, not the last.
finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g) 
finiteRandoms 0 g = ([], g) 
finiteRandoms n g = let (num, gen) = random g 
                    in (take n $ num : randoms gen, gen)