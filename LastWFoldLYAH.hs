--LYAH
last' :: [a] -> a
last' = foldr1 (\_ x -> x) 