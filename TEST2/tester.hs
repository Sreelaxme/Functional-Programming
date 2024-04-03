sort :: Ord a =>  [a] -> [a]
sort [] = []
sort (x:xs) = lt ++ (x : rt)
            where   lt = [y | y<-xs, y <= x ]
                    rt = [y | y<-xs, not (y<=x) ] 


data DayOfWeek = Sun
                | Mon
                | Tue
                | Wed
                |Thu
                | Fri
                | Sat
enum :: DayOfWeek -> Int
enum Sun = 0
enum Mon = 1
enum Tue = 2
enum Wed = 3
enum Thu = 4
enum Fri = 5
enum Sat = 6

instance Eq DayOfWeek where 
    (==) x y = (enum x) == (enum y)
instance Ord DayOfWeek where 
    (<=) x y = (enum x) <= (enum y)

getInt :: IO Int
getInt = fmap read getLine 

inc :: IO Int 
inc = fmap (+1) getInt 

data Person a = Person String Int a | NotPerson

instance Eq (Person a)where 
    -- (==) :: Person a -> Person b -> Bool
    (==) (Person _ x _) (Person _ y _) = x == y

foo :: Person a-> Person a -> Bool
foo x y = x == y

data Result err a = Err err | Ok a 

instance Functor (Result err) where 
    -- fmap :: (a0->b0) -> Result err a0 -> Result err b0
    fmap f (Err e) = Err e
    fmap f (Ok a) = Ok (f a) 

instance Applicative (Result err) where 
    -- pure :: a -> Result err a
    pure = Ok  
    -- <*> :: Result (a0 -> b0) -> Result err a0 -> Result err b0
    Err e <*> _ = Err e
    Ok f <*> r = fmap f r