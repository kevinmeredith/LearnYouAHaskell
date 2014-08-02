--Problem 8. Find the greatest product of five consecutive digits in the 1000-digit number.
--TODO: Copy the long number from Project Euler site

multN :: Int -> [Integer] -> Maybe Integer
multN _ [] = Nothing 
multN n xxs@(_:xs) = Just $ multN' n xs $ productN n xxs
    where multN' _ [] acc = acc
          multN' n yys@(_:ys) acc = multN' n ys $ max (productN n yys) acc
          productN n zs = product $ take n zs

maxConsecutiveProd :: Int -> [Integer] -> Maybe Integer
maxConsecutiveProd _ [] = Nothing
maxConsecutiveProd n xs = multN n xs