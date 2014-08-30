import Text.Read
import Control.Monad
import Data.List

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x*y):ys)
foldingFunction (x:y:ys) "-" = return ((x-y):ys)
foldingFunction (x:y:ys) "+" = return ((x+y):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
	[result] <- foldM foldingFunction [] (words st)
	return result