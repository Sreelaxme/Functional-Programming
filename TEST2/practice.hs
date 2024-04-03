class Eq a where
    == :: a -> a -> Bool
    /= :: a -> a -> Bool

    == x y = not $ x /= y 
    /= x y = not $ x == y -- default

instance Eq Int where 
    -- == :: a -> a -> Bool
    (==) = eqInt16 
    -- meh some memory comparison

instance (Eq a, Eq b) => Eq (a,b) where 
    (==) (x1,x2) ( y1,y2) = (x1 == y1) && (x2 == y2)


-----------------MAYBE --------------------------
data Maybe a = Just a | Nothing   

instance Applicative Maybe where
    pure = Just
    Just f <*> Just x = Just (f x )
    _ <*> _ = Nothing

 
instance Eq a => Eq (Maybe a) where 
    eq :: Maybe a -> Maybe a -> Bool
    eq (Just a) (Just b) = (a == b)
    eq Nothing Nothing = True
    eq _ _ = False
-------------------LIST-------------------------

instance Eq a => Eq [a] where
    eq :: [a] -> [a] -> Bool
    eq [] [] = True
    eq (x:xs) (y:ys) = (x==y) && (xs == ys)
    eq _ _ = False 

-------------------------------------------------------------
hash :: String -> Hash
data HString = HString Hash String
-------------------------------------
class Eq a => Ord a where 
    (<=) :: a -> a -> Bool

-- instance Ord a 
------------------QUICK SORT----------------------------------------
sort :: Ord a =>  [a] -> [a]
sort [] = []
sort (x:xs) = lt ++ [x] ++ rt
            where   lt = [y | y<-xs, y <= x ]
                    rt = [y | y<-xs, not (y<=x) ] 

--------------------------------------------------------------
class Functor t where 
    fmap :: (a->b) -> t a -> t b


instance Functor Either a where 
    -- fmap :: (a0 -> b) -> Either a a0 -> Either a b
    fmap _ Left x = Left x 
    fmap f Right x = Right (f x )

--------------------ZIPLIST---------------------------------
newtype ZipList a = ZipList [a]

instance Functor ZipList where 
    fmap f ZipList xs = ZipList fmap f xs

instance Applicative ZipList where 
    pure a = ZipList [a]
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    <*> (ZipList f) (ZipList x) = ZipList ( zipWith ($) f x ) 
------------------------------------------------------------
getInt :: IO Int
-- getLine :: IO String
getInt = fmap read getLine 

inc :: Int -> Int 
inc = fmap (+1) getInt 
--------------------------------------------------------------
data Exp a = Exp a | Not a | NoExp
instance Functor Exp where
    -- fmap :: (a->b) -> Exp a -> Exp b
    fmap f (Exp a)  = Exp (f a)
    fmap f (Not a) = Not (f a)
    fmap _ NoExp  = NoExp

instance (Functor Exp)=>Applicative Exp where 
    -- pure :: a -> Exp a 
    pure  = Exp
    -- <*> :: Exp (a -> b) -> (Exp a) -> (Exp b)
    NoExp <*> _ = NoExp
    _ <*> NoExp = NoExp
    Exp f <*> e = fmap f e
----------------------------------------------------------------

data Person a = Person String Int a | NotPerson

instance Functor Person where 
    -- fmap :: (a->b) -> Persom a -> Person b
    fmap _ NotPerson = NotPerson
    fmap f (Person name age x) =  Person name age (fx)

instance Applicative Person where 
    -- pure :: a -> Person a 
    pure = Person "" 0 
    -- <*> :: Person (a -> b ) -> Person a -> Person b
    <*> (Person name1 age1 f) (Person name2 age2 x)= Person name1++name2 age1++age1 (f x)  
---------------APPLICATIVE----------------
class Functor t => Applicative t where 
    pure :: a -> t a 
    (<*>) :: t ( a -> b) -> t a -> t b

------------------------------------------
data Expr =  C Int
            | Plus Expr Expr
            | Sub  Expr Expr
            | Mul  Expr Expr
            | Div  Expr Expr

eval :: Expr -> Maybe Int 
eval (C x) = Just x 

eval (Plus e1 e2 ) = let mv1 = eval e1 
                        mv2 = eval e2
                    in 
                        case mv1 of 
                            Nothing -> Nothing
                            Just x -> case mv2 of 
                                Nothing -> Nothing 
                                Just y -> Just (x+y)

----------------EITHER -----------------
data Either  a b= Left a | Right b

instance Applicative Either a where 
    -- pure :: a -> Either a 
    pure = Right
    -- <*> :: Either (a0->b0) -> Either a a0-> Either a b0
    Right f <*> ex  = fmap f ex
    Left e <*>      =  Left e


------------LIST-------------------
instance Applicative [] where
    -- pure :: a -> []
    -- (<*>) :: [a->b] -> [a] -> [b]
    pure x = [x]
    (<*>) (fs) (xs) = [f x | f <- fs, x<-xs]

--------------------------MONADS------------------------
(>>=) :: t a -> (\a -> t b) -> t b

(<*>) tf ta = tf >>= (\f -> ta >>= (\a -> pure (f a)))

