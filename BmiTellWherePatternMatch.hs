-- from LYAH
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height 
   | bmi <= skinny = "skinny!"
   | bmi <= normal = "normal!"
   | bmi <= fat    = "Fat!"
   | otherwise     = "HUDGE"
   where bmi = weight / height ^ 2
         (skinny, normal, fat) = (18.5, 25.0, 30.0)