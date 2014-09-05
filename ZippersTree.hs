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

-- re-writing to be safe
maybeElemAt :: Directions -> Tree a -> Maybe a  
maybeElemAt []     (Node x _ _)     = Just x
maybeElemAt (R:ds) (Node _ _ Empty) = Nothing
maybeElemAt (R:ds) (Node _ _ r)     = maybeElemAt ds r
maybeElemAt (L:ds) (Node _ Empty _) = Nothing
maybeElemAt (L:ds) (Node _ l _)     = maybeElemAt ds l
