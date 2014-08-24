import Control.Monad.Trans.State
import qualified Data.Map as M

action = do
	modify (M.insert "x" 2)
	modify (M.insert "y" 3)
	x <- gets (M.! "x")
	y <- gets (M.! "y")
	modify (M.insert "z" (x+y))
	modify (M.adjust (+2) "z")
	gets (M.! "z")

main = do
  let (result, vars) = execState action M.empty

  putStr "Result: "
  print result

  putStr "Vars:" 
  print vars	