--Reverse Polish Notation
--solveRPN :: (Num a) => String -> a
solveRPN :: String -> String
solveRPN xs = head $ foldl (\acc x -> process acc x) [] $ words xs

process :: [String] -> String -> [String]
process acc elem 
 | isOp elem = reverse $ calculate (e1, elem, e2) : (drop 2 . reverse $ acc)
 | otherwise  = reverse $ elem : acc 
 where (e1, e2) = first2 acc

first2 :: [a] -> (a, a)
first2 (x:y:_) = (x,y)

calculate :: (String, String, String) -> String
calculate (x, op, y) 
 | op == "+" = show $ (read x :: Double) + (read y :: Double)
 | op == "-" = show $ (read x :: Double) - (read y :: Double)
 | op == "*" = show $ (read x :: Double) * (read y :: Double)
 | op == "/" = show $ (read x :: Double) / (read y :: Double)

isOp :: String -> Bool
isOp x = elem x ["+", "-", "*", "/"]


-- "10 4 3 + 2 * -"