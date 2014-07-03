-- StackExchange answer - http://codereview.stackexchange.com/questions/55752/reverse-polish-notation-calculator
import System.Environment

manipulateStack :: [Double] -> String -> [Double]
manipulateStack stack s
 | s == "+"    = (next + top) : stack''
 | s == "-"    = (next - top) : stack''
 | s == "*"    = (next * top) : stack''
 | s == "/"    = (next / top) : stack''
 | s == "^"    = (next ** top) : stack''
 | s == "sqrt" = (sqrt top) : stack'
 | s == "sin"  = (sin top) : stack'
 | s == "cos"  = (cos top) : stack'
 | s == "tan"  = (tan top) : stack'
 | s == "pi"   = pi : stack
 | s == "e"    = (exp 1) : stack
 | otherwise   = (read s::Double) : stack
 where top     = head stack
       stack'  = tail stack
       next    = head stack'
       stack'' = tail stack'

rpn :: [Double] -> [String] -> [Double]
rpn = foldl manipulateStack

solveRPN :: [String] -> Double
solveRPN s = head $ rpn [] s

main = do 
	args <- getArgs
	putStrLn $ show $ solveRPN args

