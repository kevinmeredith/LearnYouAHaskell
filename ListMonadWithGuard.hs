-- *Main Control.Applicative Control.Monad> [ x | x <- [1..50], '7' `elem` show x]
-- [7,17,27,37,47]

import Control.Monad

include7 :: [Int]
include7 = do
	x <- [1..50]
	if ('7' `elem` show x) then [x] else []

-- ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]	

--LYAH
sevensOnly :: [Int]
sevensOnly = do
	x <- [1..50]
	guard ('7' `elem` show x)
	return x
