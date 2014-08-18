import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
	a <- (*2)
	b <- (+10)
	return (a+b)

-- help from here (http://stackoverflow.com/a/25255479/409976) 
-- and friend for using `return` 

addStuff' :: Int -> Int
addStuff' = (*2) >>= (\x -> (+10) >>= (\y -> return (x + y)))