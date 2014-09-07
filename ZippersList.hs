data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

type BreadCrumb a = List a

-- zipper function for a List
stepOne :: (List a, BreadCrumb a) -> (List a, BreadCrumb a)
stepOne (Cons x xs, cs) = (xs, Cons x cs)

--ghci> let list = Cons 5 (Cons 10 Empty)
--ghci> list
--Cons 5 (Cons 10 Empty)
--ghci> stepOne (list, Empty)
--(Cons 10 Empty,Cons 5 Empty)