class Applicative t => Monad t where 
    return :: a -> t a
    (>>=) :: t a -> (a -> t a) -> t b

-- (<*>) :: t (a->b) -> t a -> t b
-- tf :: t (a->b)
-- ta :: t a
-- (<*>) tf ta = ta >>= (\f -> ta >>= (\x -> pure  $ f x))

