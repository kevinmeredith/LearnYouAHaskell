-- LYAH
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
   where bmi weight height = weight / height ^ 2