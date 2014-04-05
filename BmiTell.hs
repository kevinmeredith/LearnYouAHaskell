bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
      | bmi <= 18.5 = "You're underweight!"
      | bmi <= 25.0 = "You're normal!"
      | bmi <= 30.0 = "you're fat!"
      | otherwise   = "you're a whale!"