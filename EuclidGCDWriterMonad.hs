import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b 
  | b == 0 = do
  	tell ["finished with " ++ show a]
  	return a
  | otherwise = do
  	tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
  	gcd' b (a `mod` b)