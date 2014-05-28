-- LYAH until `treeInsert after EmptyTree`

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
  | x > a && right == EmptyTree = Node a left (singletonTree x)
  | x < a && left == EmptyTree  = Node a (singletonTree x) right
  | x > a                       = Node a left (treeInsert x right) 
  | otherwise                   = Node a (treeInsert x left) right