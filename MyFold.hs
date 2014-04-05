-- implement fold in haskell.

myFold :: [a] -> (a -> [b]) -> [b] -> [b]
myFold [] f acc = acc
myFold (x:xs) f acc = myFold xs f (acc ++ f x)