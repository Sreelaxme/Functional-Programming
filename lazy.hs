zipWith :: (a->b->c) -> [a] -> [b] -> [c]

zipWith f (x:xs) (y:ys) = f x y : Main.zipWith f  xs ys
zipWith _ _ _ = []

add u v = u+v

tl :: [a] -> [a]
tl (x:xs) = xs

-- FIBONACCI--
fib = 1:1: Main.zipWith (+) fib (tl fib)      

--------- PRIMES

_sieve :: [Int] -> [Int]

_sieve (x:xs) = x : _sieve (_strikeOff x xs)

_primes = _sieve [2..100]

_strikeOff x xs = _remove (_multiples x) xs

_multiples x = [k*x | k <- [2..]]

_remove (r:rs) (y:ys) | y>r = _remove rs (y:ys)
                    | r == y = _remove rs ys
                    | otherwise = y: _remove (r:rs) ys