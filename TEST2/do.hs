-- class Applicative t => Monad t where 
--     return :: a -> t a
--     (>>=) :: t a -> (a -> t a) -> t b


-- getLine :: IO String
get :: Read a => IO a
-- fmap :: Functor f => (a->b) -> f a -> f b
get = fmap read getLine

-- Read integer from user, increment and print it

-- class Show a where 
--     show :: a -> String 

-- putStrLn :: String -> IO ()

-- print :: Show a => a -> IO ()
-- putStr :: String -> IO()

-- print x = putStr (show x) 
-- print = putStr . show
main :: IO ()
main = do 
            putStrLn "Give an integer"
            inp <- get
            print (inp+1 :: Int)

-- Infinite times sqaure ---------------------------
square = do
            putStrLn "Enter a Number"
            inp <- get
            print (inp*inp)
            square

foo = do 
        return "Hello"
        return "World"

test1 = do
            x <- foo
            putStrLn x

-- <$> :: fmap :: (a->b) -> f a -> f b
-- (++) :: (string -> string -> string)
-- so (++) is mapped to getLine ...then it is applied on getLine
-- (string -> string -> string) -> IO String -> IO (String -> String) -> IO String -> IO String
test2 = (++) <$> getLine <*> getLine

-- putStrLn >> get >>= (\inp -> putStrLn (show(inp+1)))
-- putStrLn >> get >>= ( \inpt -> print (inp+1))
------------------------NOTES--------------------------------------

-- Applicative in terms of binding 

-- (<*>) :: t (a->b) -> t a -> t b
-- -- tf :: t (a->b)
-- -- ta :: t a
-- (<*>) tf ta = ta Main.>>= (\f -> ta Main.>>= (\x -> pure  $ f x))

--The do Notation

-- stmt = do action;stmt
--         | do x <- action ; stmt
--         | action

-- (>>) :: t a -> t b -> t b

-- do f <- tf
--     x <- ta 
--     return f 


---------------------------------
-- Equational reasoning is possible ie foo = getLine means foo can be replaced everwhere with getLine an
-- and vice versa

