-- make a function that simulates tossing a function 3 times
import System.Random

tossCoin3 :: Int -> (Bool, Bool, Bool)
tossCoin3 x = 
	let (num1, gen1) = random (mkStdGen x)   :: (Int, StdGen)
	    (num2, gen2) = random (gen1)         :: (Int, StdGen)
	    (num3, _)    = random (gen2)         :: (Int, StdGen)

	in (convertToBool num1, convertToBool num2, convertToBool num3)
    where convertToBool y = if y `mod` 2 == 0 then True else False
