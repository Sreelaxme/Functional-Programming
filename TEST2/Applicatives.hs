class Functor t => Applicative t where
    pure a :: a -> t a 
    (<*>) :: t (a -> b) -> t a -> t b
    --liftA2 :: (a -> b -> c) -> f a -> f b -> f c 
    -- liftA2 f x y = f <$> x <*> y
    -- In particular, if fmap is an expensive operation, 
    -- it is likely better to use liftA2 than to fmap over the structure and then use <*>.
instance Applicative ZipList where 
    -- (<*>) :: ZipList (a->b) -> ZipList a -> ZipList b
    (<*>) (ZipList fs) (ZipList xs) = ZipList (zipWith ($) fs xs)

-- instance Applicative ZipList where
--     pure x = ZipList (repeat x)
--     liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

instance Applicative Maybe where 
    -- pure :: a -> Maybe a
    -- (<*>) :: Maybe(a->b) -> Maybe a -> Maybe b
    pure  = Just 
    (<*>) (Just f) (Just x) = Just (f x)
    (<*>) _ _               = Nothing

instance Applicative (Either a) where
    -- pure :: a0 -> Either a a0
    -- (<*>) :: Either(a0->b) -> Either a a0 -> Either a b
    pure x = Right x 
    (<*>) Left e _ = Left e
    (<*>) (Right f) r = fmap f r 
 
instance Applicative Tree where
    -- pure :: a -> Tree a
    -- (<*>) :: Tree (a->b) -> Tree a -> Tree b
    pure x = Node Empty x Empty
    (<*>) (Node f1 f f2) (Node l x r) = Node (f1 <*> l) (f x) (f2 <*> r)

-- all possible combinations
instance Applicative [] where
    -- pure :: a -> []
    -- (<*>) :: [a->b] -> [a] -> [b]
    pure x = [x]
    (<*>) (fs) (xs) = [f x | f <- fs, x<-xs]

---------------------------------------------------------

data Exp = C Int
        |PLUS Exp Exp
        | DIV Exp Exp

-- eval :: Exp -> Maybe Int
-- eval (C x) = Just x
-- eval (PLUS e1 e2) = case (eval e1, eval e2) of
--                       (Just x, Just y) -> Just (x + y)
--                       _                -> Nothing
-- eval (DIV e1 e2) = case (eval e1, eval e2) of
--                      (Just x, Just y) -> if y /= 0 then Just (x `div` y) else Nothing
--                      _                -> Nothing

eval :: Exp -> Maybe Int
eval (C x) = Just x
eval (PLUS e1 e2) = case eval e1 of 
                        Just x -> case eval of e2
                                    Just y -> Just (x+y)
                                    Nothing -> Nothing
                        Nothing -> Nothing
eval (DIV e1 e2) = case eval e1 of 
                        Just x -> case eval of e2
                                    
                                    Just y -> if y/=0 then Just (x/y) else Nothing
                                    Nothing -> Nothing
                        Nothing -> Nothing  
