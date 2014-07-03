-- Heathrow to London

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

data Side = L | R deriving (Show)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Side, t) -> Tree (Side, t) -> Tree (Side, t)
treeInsert x@(L, _) (Node a left right) = Node a (treeInsert x left) right
treeInsert x@(R, _) (Node a left right) = Node a left (treeInsert x right)
treeInsert x EmptyTree = singletonTree x

-- simple graph -- start at top to bottom
--  0
-- /   \ 
--5     10
-- \   / 
--  0 

-- Node 0 (Node 5 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree)
