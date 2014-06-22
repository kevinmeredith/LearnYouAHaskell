-- randoms that takes a generator and returns an infinite sequence of values based on that generator
import System.Random

randoms' :: StdGen -> [Int]
randoms' g = num : randoms' gen
   where (num, gen) = random g 