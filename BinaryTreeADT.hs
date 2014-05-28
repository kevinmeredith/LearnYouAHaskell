-- LYAH 
-- I  mistakenly failed to account for `x == a` in treeInsert. Also
-- I did not need to account for left or right being EmptyTree.

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x > a  = Node a left (treeInsert x right) 
  | x < a  = Node a (treeInsert x left) right

-- I originally searched left and right sides. But good that LYAH
-- showed me the need to only search left or right.
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x > a  = treeElem x right
  | x < a  = treeElem x right