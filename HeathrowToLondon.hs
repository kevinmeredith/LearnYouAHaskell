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

-- But ... using L/R doesn't appear to work for such a scenario. In other words
-- I'm not sure how to populate the below tree with a foldr

-- A 50 --- | -- 5  
--         30       
-- B 10 --- | -- 90 

-- find greedy, shorest-path for A and B, and then pick lowest

-- entering tree manually

shortestPath2 :: Tree (Int, Char) -> (Int, [Char])
shortestPath2 EmptyTree = (0, [])
shortestPath2 (Node (c, pt) EmptyTree EmptyTree) = (c + fst (shortestPath2 EmptyTree), pt : snd (shortestPath2 EmptyTree))
shortestPath2 (Node (c, pt) EmptyTree right)     = (c + fst (shortestPath2 right), pt : snd (shortestPath2 right))
shortestPath2 (Node (c, pt) left EmptyTree)      = (c + fst (shortestPath2 left), pt : snd (shortestPath2 left))
shortestPath2 (Node (c, pt) left@(Node (lcost, _) _ _) right@(Node (rcost, _) _ _)) = 
                                 (c + fst (shortestPath2 next), pt : snd (shortestPath2 next))
                                       where next = if lcost < rcost then left else right
                                             

test3 :: (Int, String)
test3 = result 
        where path1 = (Node (50, 'A') (Node (30, 'C') (Node (90, 'D') (Node (0, 'E') EmptyTree EmptyTree) EmptyTree) EmptyTree)
                                     (Node (5, 'C') (Node (0, 'E') EmptyTree EmptyTree) EmptyTree)) 
              
              path2 = (Node (10, 'B') (Node (30, 'D') (Node (5, 'C') (Node (0, 'E') EmptyTree EmptyTree) EmptyTree) EmptyTree)
                                      (Node (90, 'D') (Node (0, 'F') EmptyTree EmptyTree) EmptyTree))

              sp1 = shortestPath2 path1
              sp2 = shortestPath2 path2
              compare first@(cost1, _) second@(cost2, _) = if cost1 < cost2 then first else second
              result = compare sp1 sp2
