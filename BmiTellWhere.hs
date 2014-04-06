-- from LYAH
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "underweight!"
  | bmi <= 25.0 = "normal, but ugly"
  | bmi <= 30.0 = "fatty"
  | otherwise = "wHALE!"
  where bmi = weight / height ^ 2