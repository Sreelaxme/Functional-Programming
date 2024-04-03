-- Take a number check whether it is prime

-- isPrime :: Int -> Bool
-- isPrime k = if k > 1 then null [x | x <- [2..ceiling (sqrt (fromIntegral k))], k `mod` x == 0] else False

-- fromIntegral converts the integer k to a floating-point type, allowing sqrt to operate on it.

get = fmap read getLine 

isPrime :: Int -> Bool
isPrime k = if k > 1 then null [x | x <- [2..k-1], k `mod` x == 0] else False

primeCheck = do
                putStrLn "Enter a number"
                inp <- get
                print $ isPrime inp
                primeCheck

-- - 
--  Give an IO action that will read two integers and compute their sum
--  (make use of getInt)

getInt :: IO Int 
getInt = fmap read getLine

foo = (+) <$> getInt <*> getInt 