data Exp v = Var v
            | Plus (Exp v) (Exp v)
            | Mul (Exp v ) (Exp v)
            | Const Int

instance Functor Exp where 
    -- fmap :: (a->b) -> Exp a -> Exp b
    fmap vT (Var x) = Var (vT x)
    fmap vT (Plus x y ) = 
-- join :: Monad m => m (m a) -> m a

-- join mma = do ma <- mma
--                 ma


join :: Exp (Exp v) -> Exp v
join (Const x) = Const x
join (Var x)  = x 
join (Plus e1 e2) = Plus (join e1) (join e2)
join (Mul e1 e2) = Mul (join e1) (join e2)
