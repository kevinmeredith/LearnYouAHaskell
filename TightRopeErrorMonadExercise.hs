-- As an exercise, you can rewrite that with the error monad so that when the 
-- tightrope walker slips and falls, we remember how many birds were on each side 
-- of the pole when he fell.

-- if there's a difference of more than 4 birds per tight-rope, 
-- then Pierre will fall

import Control.Monad.Error

type Birds = Int
type Pole = (Birds, Birds)

-- >>= :: Monad m => m a -> (a -> m b) -> m b

addLeft :: Birds -> Pole -> Either Pole Pole
addLeft n (x, y) = let newLeft = x + n
                       newPole = (newLeft, y)
                       diff    = abs (newLeft - y)
                   in if (diff > 4) then Left  newPole
                   	  else               Right newPole

addRight :: Birds -> Pole -> Either Pole Pole
addRight n (x, y) = let newRight = y + n
                        newPole = (x, newRight)
                        diff    = abs (x - newRight)
                    in if (diff > 4) then Left  newPole
                   	   else               Right newPole                   	  

walkTightRope :: Either Pole Pole
walkTightRope = addLeft 2 (0,0) >>= addRight 4

walkTightRopeFailed :: Either Pole Pole
walkTightRopeFailed = addLeft 2 (0,0) >>= addRight 10 >>= addLeft 2

walkTightRopeDo :: Either Pole Pole
walkTightRopeDo = do 
	p1 <- addLeft 2 (0,0)
	p2 <- addRight 1 p1
	return p2

