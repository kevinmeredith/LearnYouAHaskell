-- LYAH until `treeInsert after EmptyTree`

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x > a  = Node a left (treeInsert x right) 
  | x < a  = Node a (treeInsert x left) right