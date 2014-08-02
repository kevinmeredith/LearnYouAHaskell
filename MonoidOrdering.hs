import Data.Monoid

cmp :: String -> String -> Ordering
cmp [] [] = EQ
cmp [] _  = LT
cmp _ []  = GT
cmp (x:xs) (y:ys) 
  |  z == EQ   = cmp xs ys
  |  otherwise = z
  where z = compare x y


-- Let's say you were writing a function that takes two strings, compares their lengths, 
-- and returns an Ordering. But if the strings are of the same length, then instead of 
-- returning EQ right away, we want to compare them alphabetically

--LYAH

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in  if a == EQ then b else a

-- rewrite with monoids
lengthCompareM :: String -> String -> Ordering
lengthCompareM x y = let a = length x `compare` length y
                         b = x `compare` y
                    in  a `mappend` b

-- If we wanted to expand this function to also compare for the number of
-- vowels and set this to be the second most important criterion for comparison

vowelsCompare :: String -> String -> Ordering
vowelsCompare x y = let a = length x `compare` length y
                        b = vowels x `compare` vowels y
                        vowels = length . filter (`elem` "aeiou")
                    in  a `mappend` b


-- LYAH

vowelsCompareLYAH :: String -> String -> Ordering
vowelsCompareLYAH x y = (length x `compare` length y) `mappend`
                        (vowels x `compare` vowels y) `mappend`
                        (x `compare` y)
               where vowels = length . filter (`elem` "aeiou")