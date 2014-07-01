--Reverse Polish Notation
--solveRPN :: (Num a) => String -> a
solveRPN :: String -> Double
solveRPN xs = head $ foldl (\acc x -> foldingFunction acc x) [] $ words xs

foldingFunction :: [Double] -> String -> [Double]
foldingFunction acc elem 
 | isOp elem  = calculate (take 2 acc) elem : (drop 2 acc)
 | otherwise  = (read elem :: Double) : acc 
 
calculate :: [Double] -> String -> Double
calculate (y:x:_) op
 | op == "+" =  x + y
 | op == "-" =  x - y
 | op == "*" =  x * y
 | op == "/" =  x / y

isOp :: String -> Bool
isOp x = x `elem` ["+", "-", "*", "/"]


-- "10 4 3 + 2 * -"