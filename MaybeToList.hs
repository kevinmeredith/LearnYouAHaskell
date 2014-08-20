import Control.Monad

-- see http://stackoverflow.com/questions/25393698/flattening-a-list-in-scala-haskell

maybeToList :: Maybe a -> [a]
maybeToList (Just x)  = [x]
maybeToList Nothing   = []

flatten' :: [Maybe a] -> [a]
flatten' xs = xs >>= maybeToList