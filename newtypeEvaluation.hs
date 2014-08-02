newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "Hello"

--*Main> helloMe (CoolBool True)
--"Hello"
--*Main> helloMe (CoolBool False)
--"Hello"

