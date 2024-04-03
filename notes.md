## Type classes
- Checking equality between two function is essentially halting problem
- Type class mechanism is a way to restrict parametric polymorphism to a subclass of types.

- The compiler infers the constraints while type checking the
   program.  However as a good practice it is recommended that the top level functions be given explicit types.

- Eq should give an equivalence relatioship
- Total order?

## FUNCTORS 
### Properties of map
```
map id = id 
map (f . g) = map f . map g 
```

```
getLine :: IO String
read :: Read a => String -> a
```
### APPLICATIVE
- LAW
```
fmap f x = pure f <*> x 
```
```
pure f <*> x = <$> f x 
```
### Newtype
Newtype is evaluated after definiton itself
```
read :: Read a => String -> a
```
## Expressions
```Haskell

eval (Plus e1 e2) = (+) ($) (eval e1) <*> (eval e2)

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
