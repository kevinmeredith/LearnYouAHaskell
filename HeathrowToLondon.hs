-- Heathrow to London (Greedy)

import Control.Exception

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

data Side = L | R deriving (Show, Eq)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Side, t, u) -> Tree (Side, t, u) -> Tree (Side, t, u)
treeInsert x@(L, _, _) (Node a left right) = Node a (treeInsert x left) right
treeInsert x@(R, _, _) (Node a left right) = Node a left (treeInsert x right)
treeInsert x EmptyTree = singletonTree x

shortestPath :: (Ord u) => Tree (Side, t, u) -> Tree (Side, t, u) -> [t]
shortestPath EmptyTree EmptyTree = []
shortestPath EmptyTree (Node (R, pt, cost) left right) = pt : (shortestPath left right)
shortestPath (Node (L, pt, cost) left right) EmptyTree = pt : (shortestPath left right)
shortestPath (Node (L, pt1, cost1) l1 r1) (Node (R, pt2, cost2) l2 r2) = lowerCostPt : (shortestPath nleft nright)
                                 where (lowerCostPt, nleft, nright) = if cost1 < cost2 then (pt1, l1, r1) else (pt2, l2, r2)

-- test 1
-- simple graph -- start at top to bottom
--   start
-- /      \ 
--5(a)     100(b)
-- \      / 
--  finish 

test1 :: String
test1 = result -- should be "b"
        where xs = [(L, 'b', 5), (R, 'c', 100)]
              paths = foldr treeInsert (singletonTree (L, 'a', 0)) xs
              result = shortestPath (getLeft paths) (getRight paths)
              getLeft (Node x left _) = left
              getRight (Node x _ right) = right

-- test 2
-- simple graph -- start at top to bottom
--   start
-- /      \ 
--99(b)   100(c)
-- |       | 
-- 5(d)    |
--   \    /
--    finish

test2 :: String
test2 = result -- should be "bd" due to greedy nature
        where xs = [(L, 'd', 3), (L, 'b', 99), (R, 'c', 100)]
              paths = foldr treeInsert (singletonTree (L, 'a', 0)) xs
              result = shortestPath (getLeft paths) (getRight paths)
              getLeft (Node x left _) = left
              getRight (Node x _ right) = right

-- simplified Heathrow to London:

-- But ... using L/R doesn't appear to work for such a scenario:

-- A 50 --- | -- 5  
--         30       
-- B 10 --- | -- 90 

-- tree form:

--                 _
--              /     \ 
--             50      10
--            /  \     /  \ 
--           30   5   30   90