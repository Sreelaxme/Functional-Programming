type CalcM = StateT Env IO 
newtype StateT s m a = StateT {runStateT:: s-> m (a,s)}
newtype Identity a = Identity {runIdentity :: a}

type State s a = StateT s Identity
instance Functor m => Functor StateT s m where 
    -- fmap :: (a->b) -> (StateT s m a) -> (StateT s m b)
    fmap f sma = StateT fn 
                where --fn :: s->m (b,s)
                fn s0 = fmap (\(x,s)->(f x ,s)) runStateT sma s0 

instance Monad m => Applicative StateT s m where 
    -- pure :: a -> StateT s m a 
    pure x = StateT fn 
            where fn s0 = pure (x,s0)
            -- this pure is of Applicative m 
    -- (<*>) :: StateT s m (a->b) -> StateT s m a -> StateT s m b
    (<*>) smf sma = StateT fn
                    where fn s0 = do 
                                    (f,x1)<-runStateT smf s0
                                    (a,x2)<-runStateT sma s1
                                    return (f a , x2)

instance Monad m => Monad StateT s m where 
    --(>>=) :: StateT s m a -> (a->StateT s m b) -> StateT s m b
    (>>=) sma f = StateT fn 
    -- fn :: m(b,s)
                    where 
                        fn s0 = do 
                                (a,s1) <- runStateT sma s0
                                runStateT (f a) s1 
                                

get :: Monad m => StateT s m s
get = StateT fn
        where fn s0 = pure (s0,s0)

put :: Monad m => s -> StateT s m ()
put s = StateT fn
        where fn _ = return ((),s)


lift :: m a -> StateT s m a 
lift ma = StateT fn
            -- where fn s0 = do
            --                 a <- ma
            --                 return (a,s0)
            where fn s0 = fmap (\x -> (x,s0)) ma

