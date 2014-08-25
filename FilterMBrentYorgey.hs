-- typed out from his blog
-- http://byorgey.wordpress.com/2007/06/26/deducing-code-from-types-filterm/
import Control.Monad

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = 
	let rest = filterM' p xs in 
	  do b <- p x
	     if b then liftM (x:) rest
	     	  else            rest