-- getLine :: IO string

-- read :: Read a => String -> a

-- Read a string from IO and convert to Int

-- Class Functors t where
--     fmap :: (a -> b) -> t a -> t b 

--FUNCTOR/APPLICATIVE/MONAD instances for IO

-- instance Functor IO where
--     fmap f x = x >>= (pure . f)


getIntLine :: IO Int
getIntLine = fmap read getLine

------------------------------------------------------------------------------


--  Person with just NAME

-- data Person = Person String
-- instance Show Person where
--     show (Person s) = show s
-- getPerson :: IO Person
-- getPerson = fmap Person getLine

data Person = Person String Int
-- Person :: String -> Int -> Person

instance Show Person where
    show (Person str age) = "Name "++ show str ++ " Age "  ++show age
-- get :: IO(Int->Person)
get = fmap Person getLine


-- (<*>) :: t(a -> b) -> t a -> t b            [t is functor]
-- newGet :: IO
newGet = get <*> getIntLine

-- FullName :: String -> String -> FullName
data FullName = FullName String String

instance Show FullName where
    show (FullName a b) = show a ++ " " ++ show b

getFullName = fmap FullName getLine <*>getLine
--------------------------------------------------------------
add3 x y z = x+y+z

-- trial :: IO(Int -> Int -> Int)
trial = fmap add3 getIntLine 

-- trial2 :: IO(Int -> Int)
trial2 = trial <*> getIntLine

-- trial3 :: IO Int
trial3 = trial2 <*> getIntLine

-- Hence final
sum3 = fmap add3 getIntLine <*> getIntLine <*> getIntLine
---------------------------------------------------------------

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- f <$> t x = fmap f (t x) 
-- ($) :: (a -> b) -> a -> b

-------------------------------------------------------
getInt :: IO Int
getInt = read <$> getLine 
-- readIntList ::  IO [Int]
readInt :: Int -> IO [Int] -- Add an argument for the number of integers to read
readInt n | n <= 0    = pure []
          | otherwise = do 
                          x <- getInt
                          xs <- readInt (n-1)
                          pure (x:xs)

_sum :: Int
       
_sum =  do  n  <- getInt
            xs <- readInt n
            s <- pure $ _sum xs
            print s 
