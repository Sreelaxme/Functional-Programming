
newtype State s a = State {runState :: s->(a,s)}

-- runState :: (State s a) -> s -> (a,s)

instance Functor (State s) where 
    -- fmap :: (a->b) -> (State s a) -> (State s b)
    fmap f sa = State fn
                where fn s0 = case runState sa s0 of 
                                (a,s1) -> (f a , s1)
instance Applicative (State s) where 
    -- (<*>) :: State s (a->b) -> State s a -> State s b
    (<*>) sf sa = State fn
                    where 
                        fn s0 = case runState sf s0 of 
                            (f,s1) -> case runState sa s1 of 
                                (a,s2) -> (f a,s2)
    pure x = State fn
            where fn s0 = (x,s0)
instance Monad (State s) where 
    -- (>>=) :: (State s a ) -> (a -> State s b) -> State s b
    (>>=) sa fn = State g 
                    where -- g :: s -> (b,s)
                        g s0 = case runState sa s0 of 
                                (a,s1) -> runState (fn a) s1 

get:: State s s 

get = State fn
    where fn s0 = (s0,s0)

put :: s -> State s ()
put x = State fn 
        where fn x = ((),x)