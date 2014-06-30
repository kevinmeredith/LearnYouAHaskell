import System.Random
import Control.Monad(when)

main = do
	gen <- getStdGen
	askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do 
	let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
	putStr "which number in the range from 1 to 10 am I thinking of?"
	numberString <- getLine
	when (not $ null numberString) $ do
		let number = read numberString
		if randNumber == number
			then putStrLn "You win!"
			else putStrLn $ "sorry, it was" ++ show randNumber
		askForNumber newGen	