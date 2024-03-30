## Expressions
```Haskell
data Exp v = Var v
            | Plus (Exp v) (Exp v)
            | Mul (Exp v ) (Exp v)
            | Const Int
do x <- e1
    e2

e1 >>= \x -> e2
```
Side effects can be joined ? to make the bigger side efect
```Haskell
join :: Monad m => m (m a) -> m a

join mma = do ma <- mma
                ma

mma >>= \ma -> ma 
```
### Bind in terms of join

```Haskell 
class Functors t where
    fmap :: (a->b) -> t a -> t b
```

```Haskell
(>>=) ::Monad m =>  m a -> (a -> m b) -> m b

m a -> (a -> b') -> b' where b' is m b

(>>=) ma f = join (fmap f ma)

```

### Tree where variables themselves are trees

Syntactic monad
```
join :: Exp (Exp v) -> Exp v
join (Const x) = Const x
join (Var x)  = x 
join (Plus e1 e2) = plus (join e1) (join e2)
join Mul e1 e2 = Mul (join e1) (join e2)
````

Cont 5 is of type Exp Exp Int 
