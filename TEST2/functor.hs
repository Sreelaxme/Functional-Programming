-- writing Functors so that no conflict occurs with the GHC.Base

-- Refer GHC.Base documentation
class Functors t where
    fmap :: (a->b) -> t a -> t b
    -- (<$) :: a -> f b -> f a

-- >>> 'a' <$ Just 2
-- Just 'a'
-- >>> 'a' <$ Nothing
-- Nothing

--------------- map for list--------------------
-- map :: (a->b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = (f x) : (map f xs) 
instance Functors [] where 
    fmap = map

-----------PROPOERTIES-------------------
-- fmap id = id
-- fmap f . fmap g = fmap (f . g)
--      (.) :: (b -> c) -> (a -> b) -> a -> c
-----------------------------------------

instance Functors Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- ($):: (a->b) -> a -> b
-- Just (f x) is Just $ f x 

data Tree a = Empty
            | Node (Tree a) a (Tree a)

instance Functors Tree where
    fmap _ Empty = Empty
    fmap f (Node l a r) = Node (fmap f l) a (fmap f r) 

instance Functors (Either a) where
    -- fmap :: (a0 -> b) -> Either a a0 -> Either a b
    fmap f (Right x)    = Right (f x)
    fmap f (Left x)     = Left x

newtype ZipList = ZipList {getZipList :: [a]}

instance Functors ZipList where
    fmap f (ZipList xs) = ZipList (fmap f xs)

instance Functors IO where
    fmap f ioAction = do
        result <- ioAction
        return (f result)

-- instance Functor T where
--   fmap f x = do
--     x' <- x
--     return (f x')

-- instance Applicative T where
--   pure = return
--   f <*> x = do
--     f' <- f
--     x' <- x
--     return (f' x')