-- if a list has at least two elements, return True, else False
import Data.Maybe
import Network.CGI.Protocol

matchHeadTwo :: [a] -> Bool
matchHeadTwo (x:y:_) = True
matchHeadTwo _       = False

-- extract maybeHead of Maybe [a]
safeHead :: Maybe [a] -> Maybe a
safeHead m = do
	x <- m -- x has type [a]
	listToMaybe x

maybeStringToMaybeDouble :: Maybe String -> Maybe Double
maybeStringToMaybeDouble x = do
	  y <- x
	  maybeRead y 
