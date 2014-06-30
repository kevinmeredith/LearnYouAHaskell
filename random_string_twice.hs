-- print out 2 instances of random strings. LYAH points out that
-- simply calling `getStdGen`, followed by take 20 $ ... won't work
-- since the same `StdGen` will be returned.
import System.Random

main = do
   gen <- getStdGen
   let rands = randomRs ('a', 'z') gen
       (first20, rest) = (take 20 $ rands, drop 20 $ rands)
       in putStrLn $ first20 ++ "\n" ++ (take 20 $ rest) 