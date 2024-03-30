class Applicative t => Monad t where
    return :: a -> t a 
    (>>=) :: t a -> (a ->  t b) -> t b


instance Monad Maybe where 
    -- return :: a -> Maybe a 
    return  = Just  
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) Nothing _ = Nothing
     Just x >>= f = f x 

instance Monad Either a where 
    -- return :: b -> Either a b
    return = Either a 
    -- (>>=) :: Either a a0 -> (a0 - > Either a b) -> Either a b
    Left l >>= _ = Left l
    Right r >>= f = f r 

-- data Tree a = Empty | Node (Tree a ) a (Tree a)
instance Monad Tree where 
    -- return :: a -> Tree a
    return x = Node Empty x Empty
    -- (>>=):: Tree a -> (a -> Tree b) -> Tree b
    (>>=) Empty _ = Empty
    Node left x right >>= f = Node (left >>= f) (f a) (right >>= f)


instance Monad [] where
    -- return :: a -> [a]
    return x = [x]
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concat (map f xs)

ZipList a = ZipList {getZipList ::[a]}
instance Monad ZipList where 
    return = pure
    -- (>>=) :: ZipList a -> (a -> ZipList b) -> ZipList b
    ZipList xs >>= f = ZipList $ concatMap (getZipList . f) xs 
    -- concatMap applies the function to each element of the structure, and then concatenates 
    -- the resulting lists into a single list.
    -- concatMap (getZipList . f) xs: This part applies the function f to each 
    -- element of the list xs, resulting in a list of ZipLists. 
    -- The getZipList function is used to extract the underlying list from each ZipList in the result.
