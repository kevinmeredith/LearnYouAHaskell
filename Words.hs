-- added `Data.Char.isSpace` per http://codereview.stackexchange.com/a/49696/31595
import Data.Char 

words' :: String -> [String]
words' []  = []
words' xxs@(x:xs) 
  | isSpace x = words' xs
  | otherwise = ys : words' rest
                  where (ys, rest) = break isSpace xxs

-- ghci> words "hey these are the words in this sentence"  
-- ["hey","these","are","the","words","in","this","sentence"]  
-- ghci> words "hey these           are    the words in this\nsentence"  
-- ["hey","these","are","the","words","in","this","sentence"]  