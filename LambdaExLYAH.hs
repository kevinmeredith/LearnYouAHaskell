--LYAH
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x 
 | odd x  = x : chain (x*3 + 1)
 | even x = x : chain (x `div` 2)