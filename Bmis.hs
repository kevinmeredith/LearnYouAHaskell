-- from LYAH, but modified to a tuple
bmiTell :: (RealFloat a) => (a, a) -> String
bmiTell (weight, height)
  | bmi <= 18.5 = "underweight!"
  | bmi <= 25.0 = "normal, but ugly"
  | bmi <= 30.0 = "fatty"
  | otherwise = "wHALE!"
  where bmi = weight / height ^ 2

-- apply bmis to a list of pairs
bmis :: (RealFloat a) => [(a, a)] => [String]
bmis xs = map bmiTell xs