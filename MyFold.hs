-- implement fold in haskell.

go :: [a] -> (a -> [b]) -> [b] -> [b]
go [] f acc = acc
go (x:xs) f acc = go xs f (acc ++ f x)

myFold :: [a] -> (a -> [b]) -> [b]
myFold as f = go as f []


 

