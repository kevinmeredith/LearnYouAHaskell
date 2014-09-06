data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

-- change W to A?
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

-- better way? how about specifying L and R directions in the Tree

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

anotherTree :: Tree Char
anotherTree = Node 'A' (Node 'B' Empty Empty) Empty

-- given a list of Directions, return the element at the end of `Directions`
elemAt :: Directions -> Tree a -> a  
elemAt []     (Node x _ _) = x
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt (L:ds) (Node _ l _) = elemAt ds l

-- I re-wrote to use Maybe, but it wasn't an exhaustive match. See below CodeReview answer
-- for a truly safe implementation
unsafeMaybeElemAt :: Directions -> Tree a -> Maybe a  
unsafeMaybeElemAt []     (Node x _ _)     = Just x
unsafeMaybeElemAt (R:ds) (Node _ _ Empty) = Nothing
unsafeMaybeElemAt (R:ds) (Node _ _ r)     = unsafeMaybeElemAt ds r
unsafeMaybeElemAt (L:ds) (Node _ Empty _) = Nothing
unsafeMaybeElemAt (L:ds) (Node _ l _)     = unsafeMaybeElemAt ds l

--http://codereview.stackexchange.com/questions/61992/find-element-in-tree-given-a-list-of-directions/62000#62000
safeElementAt :: Directions -> Tree a -> Maybe a
safeElementAt []    (Node x _ _)  = Just x
safeElementAt (R:ds) (Node _ _ r) = safeElementAt ds r
safeElementAt (L:ds) (Node _ l _) = safeElementAt ds l
safeElementAt _ Empty             = Nothing

type Breadcrumbs = [Direction]

-- returns (left sub-tree, L appended to Breadcrumbs argument)
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

-- same as above, but to the right
goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

myTree :: Tree Char
myTree = Node 'a' (Node 'b' (Node 'd' Empty Empty) (Node 'e' Empty Empty)) 
                  (Node 'c' (Node 'f' Empty Empty) (Node 'g' Empty Empty))