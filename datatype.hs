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

_minTree :: (Ord a , Num a) => Tree a -> Tree a 
_minTree Empty = Empty
_minTree tree@(Node l a r) = Node (replicateTree minVal l) minVal (replicateTree minVal r)
                            where minVal = findMin tree

findMin :: (Ord a, Num a) => Tree a -> a
findMin Empty = error "Empty Tree"
findMin (Node Empty val Empty) = val
findMin (Node Empty val right) = min val (findMin right)
findMin (Node left val Empty) = min val (findMin left)
findMin (Node left val right) = minimum [val, findMin left, findMin right]

----------------------------------DO 1 pass
_minTree :: (Ord a) => Tree a -> Tree a 
_minTree Empty = Empty
_minTree tree = fst (_minTree' tree (findMin tree))

_minTree' :: (Ord a) => Tree a -> a -> (Tree a, a)
_minTree' Empty minVal = (Empty, minVal)
_minTree' (Node l _ r) minVal =
    let (newL, minValL) = _minTree' l minVal
        (newR, minValR) = _minTree' r minVal
        updatedMinVal = minimum [minVal, minValL, minValR]
    in (Node newL updatedMinVal newR, updatedMinVal)

findMin :: (Ord a) => Tree a -> a
findMin Empty = error "Empty tree"
findMin (Node left val right) = minimum [val, findMin left, findMin right]
-----------------------------------------


  data Tree a = Empty
              | Node (Tree a) a (Tree a)


  maxTree :: Tree Int -> Int
  maxTree Empty          = 0
  maxTree (Node lt x rt) = max (maxTree lt) (max x $ maxTree rt)


  ofShape :: Tree a -> a -> Tree a
  ofShape Empty           _ = Empty
  ofShape (Node lt _ rt)  x = Node (ofShape lt x) x (ofShape rt x)

  helper _ Empty          = (Empty, 0)
  helper x (Node lt u rt) = (Node ls x rs, lmax `max` u `max` rmax)
    where (ls,lmax) = helper x lt
          (rs, rmax)= helper x rt

  onePass t = tx
     where (tx, mx) = helper mx t
