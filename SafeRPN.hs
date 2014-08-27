import Data.Maybe
import Text.Regex
import Control.Monad
import Network.CGI.Protocol

data Operator = Add | Subtract | Multiply

-- safe Reverse Polish Notation calculator
rpn :: String -> Maybe Double
rpn xs = maybeStringToMaybeDouble result :: Maybe Double
      where result = safeHead $ foldM foldingOp [] $ words xs

--foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

maybeStringToMaybeDouble :: Maybe String -> Maybe Double
maybeStringToMaybeDouble x = do
	  y <- x
	  maybeRead y 

-- extract maybeHead of Maybe [a]
safeHead :: Maybe [a] -> Maybe a
safeHead m = do
	x <- m -- x has type [a]
	listToMaybe x

foldingOp :: [String] -> String -> Maybe [String]
foldingOp acc s 
   | isNumber s        = Just (s : acc)
   | isPlusRegex s     = performOp Add acc
   | isMinusRegex s    = performOp Subtract acc
   | isMultiplyRegex s = performOp Multiply acc
   | otherwise         = Nothing

performOp :: Operator -> [String] -> Maybe [String]
performOp o (x:y:zs) = Just $ (doOp o x y) : zs
performOp _ _        = Nothing

doOp :: Operator -> String -> String -> String
doOp Add a b       = show $ (read b :: Double) + (read a :: Double)
doOp Subtract a b  = show $ (read b :: Double) - (read a :: Double)
doOp Multiply a b  = show $ (read b :: Double) * (read a :: Double)


isNumber :: String -> Bool
isNumber = matches' mkNumberRegex

mkNumberRegex :: Regex
mkNumberRegex = mkRegex "^[0-9]+[.]?[0-9]*$"

isPlusRegex :: String -> Bool
isPlusRegex = matches' mkPlusRegex

isMinusRegex :: String -> Bool
isMinusRegex = matches' mkMinusRegex

isMultiplyRegex :: String -> Bool
isMultiplyRegex = matches' mkMultiplyRegex

mkPlusRegex :: Regex
mkPlusRegex = mkRegex "^[+]$"

mkMinusRegex :: Regex
mkMinusRegex = mkRegex "^[-]$"

mkMultiplyRegex :: Regex
mkMultiplyRegex = mkRegex "^[*]$"

matches' :: Regex -> String -> Bool
matches' r x 
   | matchRegex r x == Nothing = False
   | otherwise                 = True