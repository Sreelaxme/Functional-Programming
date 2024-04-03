data Exp v = Var v
            | Plus (Exp v) (Exp v)
            | Mul (Exp v ) (Exp v)
            | Const Int

instance Functor Exp where 
    -- fmap :: (a->b) -> Exp a -> Exp b
    fmap vT (Var x) = Var (vT x)
    fmap vT (Plus e1 e2) = Plus (fmap vT e1) (fmap vt e2) 
    fmap vT (Mul e1 e2) = Mul (fmap vT e1) (fmap vt e2) 
    fmap vT Const t = Const t 

-- join :: Monad m => m (m a) -> m a

-- join mma = do ma <- mma
--                 ma
                
join mma = mma >>= (\ma -> ma)

ma >> mb = ma >>= (_ -> mb)

join :: Exp (Exp v) -> Exp v
join (Const x) = Const x
join (Var x)  = x 
join (Plus e1 e2) = Plus (join e1) (join e2)
join (Mul e1 e2) = Mul (join e1) (join e2)

-- (>>=) in terms of join
f :: v -> Exp v
(>>=) ev  f = join ( fmap f ev) 