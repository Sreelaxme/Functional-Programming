{-  * Getting started with Haskell

This is a tutorial to get you started with Haskell. You should install
the ghc compiler for Haskell which should be packaged for your OS.
Save this tutorial program some where in your file system and load it
into your ghci interpreter by typing the following on your terminal.

$ ghci

The program will respond with the lnes of the following kind.


GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/ppk/dotfiles/rcfiles/ghci
Prelude>

The Prelude> line indicates that the interpreter is now ready to take
command from you. At this point you can enter any valid haskell
expression and it will evaluate for you. We start by loading this
file into the interpreter.

Prelude> :load tutorial.hs

The GHCi is not really an interpreter in the sense that you cannot
really type a valid Haskell program and expect it to run. What it is
is a REPL (read-eval-print-loop), in the sense that it reads haskell
expressions, evaluates it and prints the result. The actual
definitions need to be in a separate file (like in this
tutorial.hs). It then needs to be loaded. In this way it is different
from the sml-nj interpreter that you might have already seen.

-}

import Prelude hiding (curry, uncurry, map, fold, sum )

aString     = "Hello  world"
aChar       = 'H'
aBool       = True
anotherBool = False
aList       = [1,2,3]
aPair       = ("the answer to life, universe, and everything", 42)
aTriple     = (1, 2.4, "hello")
aUnit       = ()  -- empty product type


{-

What you see above are not assignments but equations. When we see a
line of code that says x = e, it is to be seen as saying that x and e
are the same things and hence one can replace the other in the program
where the definition is valid without changing the meaning. For
example if we define

-}

theAnswer = 40 + 2

{-

then it means that we can replace the occurance of theAnswer with 40 +
2 (and vice-versa) any where in the program.  A Haskell program is
essentially a sequence of such equation and should not be confused
with assignments.

-}

{-  ** Comments

You would have also realised that comments in Haskell start with a
'{-' and end with a '-}'.  These are called block comments and can be
nested (unlike in C).

-}

-- A single lines can be commented by using '--' at the begining as
-- these two lines illustrate.

{- ** Types in Haskell

Languages like C or JavaScript do not take their types very seriously
in the sense that C allows (even implicitly does) conversion between
types like int, double and pointers. Such is not the case with Haskell
and in this sense Haskell is strongly typed. The types are also
checked at compile time unlike the language like Python or ruby. In
this sense Haskell is statically type checked language. Despite these,
one almost never needs to explicitly give the type declarations. The
compiler "infers" the type using the context.  What this means is that
we get all the advantages of strong and static type checking,
i.e. safety and speed, without paying its price in verbosity.

-}

{- *** Inspecting types

You can inspect the types of an expression in the ghci interpreter by
using the command :type. Similarly the ghci command :info gives you
the details of a symbol.  After loading the code try out the the following commands.


:type aBool
:info aBool
:t ("Hello" ++ "world") -- same as :type
:i Bool  -- same as :info


The :type aBool would have resulted in ghci printing something like this.

aBool :: Bool

which is Haskell's way of saying that aBool is of type Bool.

Although, Haskell can infer the types, we can explicitly assert the
type of expressions explicitly using the notation (e :: T) in which
case Haskell does the checking. This is often required if we want to
make the type concrete. The Haskell's type inference algorithm infers
the most general type for an expression. In the case of 42 it can be
any of the numerical types. So if we want to enforce that it is Int
will need to give explicit types as the examples given below show.

-}

anInt       = 42 :: Int
aDouble     = 42.0 :: Double


{-

It is also customary to assert the types of the top level constants
although almost always it is not necessary. Apart from controlling the
polymorphism, it also is a mechanism of documenting the variable. The
type of a variable often gives a lot of information about the
variable. The types in Haskell are

1. Basic types like Int, Bool etc

2. Lists: The expression [a] denotes list of type a. The list type
          is polymorphic in its element type a.

3. Product types: (a,b), (a,b,c), (a,b,c,d) etc.

4. Function type: The functions from a type a to type b is denoted by a -> b

-}

{-  ** Type aliasing.

We can give new names to types. This is called type aliasing

-}

type Point2D = (Double, Double)

-- Unit vector along x axis
unitX :: Point2D
unitX = (1.0,0.0)


-- Unit vector along y axis
unitY :: Point2D
unitY = (0.0,1.0)



{-  ** Functions.

The first class nature of functions is one of the most important
feature of a functional programming language like Haskell. Definition
of functions are also through equations. Let us start by
defining the simplest factorial function. Since function application
is a very common operation, they have a very simple syntax: applying f
on x is just f x

-}

fact :: Integer -> Integer  -- Not really required but we give it
                            -- since fact is a top level definition.
fact 0 = 1
fact n = n * fact (n - 1)

{- *** The idea of currying.

In Haskell we only have functions with one arguments. To illustrate
let us look at the following variants of add.

-}

add :: (Int, Int) -> Int
add (u,v) = u + v

addP :: Int -> Int -> Int
addP u v  = u + v

{-

While it might appear that the above function takes two arguments,
their type will reveal that really they really are single argument
functions. The add has type add :: (Int, Int) -> Int which means that
it takes as argument of type (Int,Int), i.e. the type of pairs of Int
and produces an Int. To convince yourself load this file in ghci and
try out the following lines.

-}

-- let mypair = (2 :: Int, 3 :: Int) -- define a new pair
-- :type mypair                      -- inspect the type of mypair
-- add mypair                        -- See add only takes one argument


{-

The type of addP is a bit more interesting -- addp : Int -> Int -> Int
Recall that dom -> rng denotes the type of ally functions that as dom
as the domain and rng as the range. Also a -> b -> c should be read as
a -> (b -> c), or in other words -> associates towards the right.
Thus addP has the type Int -> (Int -> Int), which means that addP's
domain is the type of Int and its range is the type of functions from
Int -> Int. This is what addP does

1. It takes a single Int argument say x :: Intand

2. It produces a function f :: Int -> Int. The function f on input y
   returns (x+y).

Functional programmers call the variant add as the uncurried version
and addp as the curried version. The advantage of the curried version
is that we can "partially" apply addition.

-}

increment :: Int -> Int
increment = addP 1

{-

In general a function that takes n parameters of types t1, t2 .. tn
and returns a value t can be thought of as the type t1 -> (t2 ->
(.. -> tn)). This is called currying. Currying is a powerful idea that
allows such partial applications. What more we can even write
functions that convert between the two styles of multi argument
functions.

-}

curry :: ((a,b) -> c)       -- input is a uncurried function
      -> (a -> b -> c)     -- out put is the curried form.
curry   f  x y  = f (x,y)  --

-- Exercise: give the explicit types for uncurry.
uncurry f (x,y) = f x y    --


addp1 = curry  add
incr  = addp1 1
add1  = uncurry addP

{-


The functions above illustrate an important aspect of Haskell --
functions are truly first class as they can be passed to other
functions or stored in a data structure like list

-}

someIntFunctions = [addP 1, addP 2, addP 3]

{-

If functions are truely first class there should be a way of creating
functions without names. Haskell allows us to do that using what are
(now) known as anonymous functions. Such functions were first explored
by Church in his lambda calculus
(https://en.wikipedia.org/wiki/Lambda_calculus).

-}


increment1 = \ x -> x + 1

{-

The expression `\ x => (...x...)` is that function that maps its
argument x to (...x...)  With annonymous function, we can rewrite some
functions definitions. For example, the following definitions all mean
the same thing.

f a b c = e
f a b   = \ c  -> e
f a     = \ b c -> e
f       = \ a b c -> e


Exercise: Rewrite curry and uncurry using annonymous
functions. i.e. Fill in the blanks below.

curry f x = fn ....
curry f   = fn ....
curry     = .....

uncurry f = fn ....
uncurry   = .....

-}

{- * List functions and pattern matching

In ML lists are written as follows

-}

firstFewOddPrimes = [3,5,7]


{-

The x:xs denotes a list with x as its first element and xs as the
rest of the list

-}

firstFewPrimes = 2 : firstFewOddPrimes


{-

Here is an example of a list function that applies a given function to
all the elements on the list

-}

map f []       = []
map f (x : xs) = f x : map f xs


{-

Let us see what happens if we increment the primes: Silly program
but illustrates the use of map and partial application.

-}

useless       = map incr firstFewPrimes
somemorestuff = map (addP 42) firstFewPrimes -- see currying in action.


{-

 Another function that is very useful  for processing lists are folds

fold f a [b0, b1 , b2, b3 ...] = f (f a b0) b1 ....

Think of op as an operator we have

fold op a [b0,b1,b2 ...] = ((a op b0) op b1) op b2 ...

corresponds to the library function foldl.

Here is the actual definition of fold.

-}


fold _ x []     = x
fold f x (y:ys) = fold f (f x y) ys


-- Let us write a function to sum up a list of numbers. Notice that
-- this is just a fold

sum = fold addP 0  -- sum [x1,x2,x3..] = ((0+x1) + x2) ....)

{-
Exercise: Our "folding" is from left, we could define a right fold as
well which does the folding from right

i.e  fold op [a,b,c,...z] b =  a op (b op ... (y op (z op b)))


If the operator o is not commutative this can be different.

The standard library has its own variants of foldr and foldl but they
accept the functions in uncurried form. Have a look at their types.

-}


{- = Parameteric Polymorphism.

We already seen the smartness of the Haskell compiler in infering the
type of your values/functions from the context. If you look at the
type that the compiler inferred (using :type map at the ghci prompt)
for your map function you would have noticed something strange.

map :: (a -> b) -> [a] -> [b]

Here the a and b are type variables, i.e. can be any type you wish. In
plain English it means the following: Let a and b be any types. The
map function takes a function a to b to a function that map an [a] to
[b]. The [a] list type itself is polymorphic and GHC has interpreted
the most general type for the map function that is consistent with its
definition. This kind of polymorphism is called parametric
polymorphism in the sense that map is a generic list maping function
that works no matter what the types a and b are.

Internally, the "most general type" is solved by solving a set of
constraints that arise by the definition. If it is not able to solve
it means that there is a type error. Here are some simple examples.

-}

identity x    = x         -- id    ::  a -> a
constant x y  = x         -- const :: a -> b -> a
compose f g x =  f (g x)  -- compose :: (b -> c) -> (a -> b) -> a -> c


{- Algebraic data types.

Algebraic data type allows us to captures abstract syntax of languages
pretty easily and thus are particularly well suited for writing
compilers. We start with few examples.

-}

data Colour = Red | Blue | Green

data DaysOfTheWeek = Sunday | Monday | Tuesday | Wednessday | Thursday | Friday | Saturday


{-

Algebraic data types capture types that are defined by a (finite) set
of rules or cases which build the type from simpler objects. The rules
or cases are distinguished by the constructors of the types. For
example, the colours datatype says that a colour is formed by three
rules distinguished by the constructors Red, Blue and Green. In the
case of DaysOfTheWeek there are 7 constructors.

The constructors have two roles. Firstly, they create values of that
type.

-}


skyColour    = Blue   -- skyColour :: Colour
favouriteDay = Sunday -- favouriteDay :: DaysOfTheWeek


{-

The constructors of a type is also used to define functions from the
type by using what is known as pattern matching as given below.

-}

isHoliday Sunday = True
isHoliday _      = False

{-

   The above function says which of the days of the week is a
   holiday. It should be read as follows as a set of two rules.

   1. Sunday is a holiday
   2. Anything else is not.


   You can think of the function as trying out each of the cases and
   seeing which of the cases match. Here, _ is a match everything
   pattern often called wildcard pattern.

-}

{-  Recursive types

   Algebraic data types can be recursive and can define very
   interesting data types. Let us define the list type ourselves even
   though the standard library already has one. Let us fix a type 'a
   (notice the polymorphism). Mathematically a list is defined using
   two rules.

   1. An empty list is a list. Call this the nil rule
   2. If xs is an 'a list and x is of type 'a then the tuple (x, xs)
      is a list. Call this the cons rule.

   We can capture this in Haskell as follows.

-}


data List a = Nil
            | Cons a (List a)


-- We can define functions again by pattern matching.
isEmpty Nil = True
isEmpty _   = False

listLength Nil          = 0
listLength (Cons x xs)  = 1 + listLength xs

{-

The listLength function can be seen as defining the length of the list
using two rules

1. The length of a nil list is 0

2. The length of a list of the kind (x,xs) is 1 + the length of the
list xs.

The only difference between our list and the standard list there is
some syntactic sugar where [] is for nil and the infix constructor :
for cons.

-}

{- In general an algebraic datatype will look like this

data Name a b c = C1 t1 t2 t2 t4 | C2 | C3 ...

Here C1, C2 C3 etc are constructors.


A pattern for the datatype would look like

1. A wild card pattern `_` (underscore) which matches any value

2. A variable pattern say `x` which like the wild card matches any
value but it also binds the value to the variable `x` so that x can be
used on the right hand side of the definition.

3. C p₁ ... pₙ where C is a constructor and p₁ ... pₙ are
   pattern. This will match any value that C e₁ ... eₙ where pᵢ
   matches the pattern eᵢ.


Just like values, the tuples and lists have special patters.

1. Built in patterns for list like ([]), (p₁ : p₂), and sequences of
   patterns like [p], [p₁,p₂], [p₁,p₂,p₃] etc.  where pᵢ's are
   themselves patterns.

2. Built in pattern for tuples like (), (p), (p₁,p₂) etc.

A function defined using a list of pattern matching rules "tries" each
pattern and gives out the value corresponding to the first case that
matched.

-}

{-
The Maybe type is a useful datatype defined in standard library that
is used to signal optional result. They capture a lot of logic that
involves the use of the unsafe null values (null pointer in C, null in
Java). They are defined as

data Maybe a = Just a | Nothing

We use this to define the head and tail of a list.

-}

head []        = Nothing
head (x:_)     = Just x

tail []        = Nothing
tail (_ : xs)  = Just xs



{-

Pattern matching using case expression.

Sometimes it is natural define values using pattern matching cases as
the following rewrite of head illustrates.

-}

head1 :: [a] -> Maybe a
head1 xs = case xs of
              []     -> Nothing
              x : _ -> Just x
