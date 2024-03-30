data Tuple a b = T a b

fst (T a _) = a 
snd (T _ b) = b

-----------------------------

data List a = Nil
            | Cons a (List a)

----------------------------

-- data BinaryTree t = Empty
--                 | Node (BinaryTree t) a (BinaryTree t)

-- binTreeOnes = Node binTreeOnes 1 binTreeOnes

data Tree t = Empty | Node (Tree t) t (Tree t)

_isEmpty Empty = True
_isEmpty _ = False

_singleTon x = Node Empty x Empty

_inorder Empty = []
_inorder (Node l a r) = _inorder(l) ++ [a] ++ _inorder(r)

-- _depth ::Tree a -> Int
-- _depth Empty = 0
-- _depth (Node l _ r) = 1+ max( depth l, depth r)

rotateAC (Node t1 a (Node t2 b t3)) = Node (Node t1 a t2) b t3

instance Show t => Show (Tree t) where
    show Empty = "Empty"
    show (Node left val right) = "Node " ++ show left ++ " " ++ show val ++ " " ++ show right


-- Helper function to replicate a tree with a specific value
replicateTree :: a -> Tree b -> Tree a
replicateTree _ Empty = Empty
replicateTree val (Node l _ r) = Node (replicateTree val l) val (replicateTree val r)

_minTree :: (Ord a) => Tree a -> Tree a 
_minTree Empty = Empty
_minTree (Node l a r) = Node (replicateTree minVal l) minVal (replicateTree minVal r)
                            where minVal = findMin (Node l a r)

findMin :: (Ord a) => Tree a -> a
findMin Empty = error "Empty Tree"
findMin (Node Empty val Empty) = val
findMin (Node Empty val right) = min val (findMin right)
findMin (Node left val Empty) = min val (findMin left)
findMin (Node left val right) = minimum [val, findMin left, findMin right]


maxTree :: Tree Int -> Int
maxTree Empty = 0
maxTree (Node lt x rt) = max (maxTree lt) (max x $ maxTree rt)


ofShape :: Tree a -> a -> Tree a
ofShape Empty           _ = Empty
ofShape (Node lt _ rt)  x = Node (ofShape lt x) x (ofShape rt x)



helper :: Int->Tree Int -> (Tree Int, Int)
helper _ Empty          = (Empty, minBound)
helper x (Node lt u rt) = (Node ls x rs, lmax `max` u `max` rmax)
        where (ls, lmax) = helper x lt
              (rs, rmax) = helper x rt


onePass ::  Tree Int-> Tree Int
onePass t = tx
    where (tx, mx) = helper mx t

-- 1. Write the fold function over the tree

foldTree :: (b->a->b->b) ->b->Tree a ->b
foldTree _ acc Empty = acc
foldTree f acc (Node l x r) = f (foldTree f acc l) x (foldTree f acc r)

sumTree = foldTree (\left val right -> left+right+val ) 0

-- 2. helper, maxTree, helper using the fold function.
maxTreeF = foldTree (\x y z -> maximum[x,y,z]) (minBound::Int)

combine ::(Ord a) => (Tree a , a, a) ->a-> (Tree a,a,a)->(Tree a,a,a)
combine (ls,lmax,mx) nodeVal (rs,rmax,_)  = ((Node ls mx rs) , lmax `max` nodeVal `max` rmax,mx)

helperF:: (Ord a,Bounded a)=>a->Tree a->(Tree a, a, a)
helperF x= foldTree combine (Empty,minBound,x) 
            
onePassF ::  Tree Int-> Tree Int
onePassF t = tx
    where (tx, mx, _) = helperF mx t

