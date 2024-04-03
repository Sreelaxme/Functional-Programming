instance Applicative Tree where
    -- pure :: a -> Tree a
    -- (<*>) :: Tree (a->b) -> Tree a -> Tree b
    pure x = infa where infa = Node infa x infa
    (<*>) (Node f1 f f2) (Node l x r) = Node (f1 <*> l) (f x) (f2 <*> r)
    (<*>) _ _ = Empty

data Result err a = Err err | Ok a 

instance Functor Result err where 
    -- fmap :: (a0->b0) -> Result err a0 -> Result err b0
    fmap f (Err e) = Err e
    fmap f (Ok a) = Ok (f a) 

instance Applicative Result err where 
    -- pure :: a -> Result err a
    pure = Ok a 
    -- <*> :: Result (a0 -> b0) -> Result err a0 -> Result err b0
    Err e <*> _ = Err e
    Ok f <*> r = fmap f r


data LeafyTree a = Empty | Leaf a | Node (LeafyTree a) (LeafyTree a)

instance Functor LeafyTree where 
    fmap f Empty = Empty
    fmap f Leaf a = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

------------------BINARY SEARCH-----------------------------------

(>=) :: a -> a-> Bool
-- search :: Ord a => Tree a -> a -> Bool
search Empty _ = False
search (Node l x r) y   |  x == y  = True
                        | x >= y  = search l y
                        | otherwise = search r y 

-------------------------
doMany :: Monad m => [m a] -> m [a]

doMany [] = pure []
doMany (x:xs) = do 
                x' <- x
                xs' <- xs
                return (x' : xs')


-----------------------------
join :: Monad m => m (m a) -> m a

join mma = do 
            ma <- mma 
            ma 

join mma = mma >>= (\ma -> ma)
            