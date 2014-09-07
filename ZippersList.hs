data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

type BreadCrumb a = List a

-- zipper function for a List
stepOne :: (List a, BreadCrumb a) -> (List a, BreadCrumb a)
stepOne (Cons x xs, cs) = (xs, Cons x cs)

type Zipper a = ([a], [a])

-- using Haskell's List
goForward :: Zipper a -> Zipper a
goForward (x:xs, cs) = (xs, x:cs)

goBackward :: Zipper a -> Zipper a
goBackward (xs, c:cs) = (c:xs, cs)

--ghci> let list = Cons 5 (Cons 10 Empty)
--ghci> list
--Cons 5 (Cons 10 Empty)
--ghci> stepOne (list, Empty)
--(Cons 10 Empty,Cons 5 Empty)