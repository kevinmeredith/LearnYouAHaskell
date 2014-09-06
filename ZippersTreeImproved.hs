data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, crumbs) = (l, LeftCrumb x r:crumbs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, crumbs) = (r, RightCrumb x l:crumbs)

myTree :: Tree Char
myTree = Node 'a' (Node 'b' (Node 'd' Empty Empty) (Node 'e' Empty Empty)) 
                  (Node 'c' (Node 'f' Empty Empty) (Node 'g' Empty Empty))

-- my implementation of `goUp`, recursively goes up until hitting an empty Breadcrumbs
-- note that the pattern match is non-exhaustive
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, [] )                = (t, [])
goUp (t, RightCrumb y l: cs) = goUp(Node y l t, cs)
goUp (t, LeftCrumb y r : cs) = goUp(Node y t r, cs)

-- LYAH's implementation
-- simply goes up once 
goUpLYAH :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUpLYAH (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUpLYAH (t, RightCrumb x l:bs) = (Node x l t, bs)

-- analogy to how a zipper behaves - zips up and down. LYAH author prefers `Focus`,
-- but Zipper already widespread
type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f 