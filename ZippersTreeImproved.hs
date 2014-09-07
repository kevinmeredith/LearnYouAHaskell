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

-- modify root of first crumb (not safe - TODO!)
modify' :: (a -> a) -> Zipper a -> Zipper a  
modify' f (t, RightCrumb x ct:crumbs) = (t, RightCrumb (f x) ct : crumbs)
modify' f (t, LeftCrumb  x ct:crumbs) = (t, RightCrumb (f x) ct : crumbs)
modify' f (Empty, bs)                 = (Empty, bs)

--  let's make a function that modifies the element in the 
--  root of the sub-tree that the zipper is focusing on
modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs)      = (Empty, bs)

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

-- So if we're focusing on an empty sub-tree, one thing
-- we can do is replace it with a non-empty subtree, thus attaching
-- a tree to a leaf node.
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- we take a tree and a zipper and return a new zipper that has its focus
-- replaced with the supplied tree. Not only can we extend trees this way
-- by replacing empty sub-trees with new trees, we can also replace whole 
-- existing sub-trees. Let's attach a tree to that far left of our `freeTree`

farLeft :: Zipper Char
farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft

newFocus :: Zipper Char
newFocus = farLeft -: attach (Node 'Z' Empty Empty)

farL :: Zipper Char
farL = (myTree, []) -: goLeft -: goLeft -: goLeft

-- walks all the way to the top of the tree, regardless of 
-- what we're focusing on
topMost' :: Zipper a -> Zipper a
topMost' (t,[])   = (t,[])
topMost' (t, LeftCrumb x r : xs) = topMost (Node x t r, xs)

-- LYAH implementation
topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z      = topMost $ goUp z