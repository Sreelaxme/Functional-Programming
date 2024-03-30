import Data.List.Split (splitOn)
import Prelude hiding (curry, uncurry, map, fold, sum )


data Tree a = Empty
             | Node (Tree a) a (Tree a)


treemap :: (a->b) -> Tree a ->Tree b
treemap f Empty = Empty
treemap f (Node l a r) = Node (treemap f l)  (f a)  (treemap f r)


{- Exercise: Rewrite curry and uncurry using annonymous
functions. i.e. Fill in the blanks below. -}

-- curry f x = \y -> f (x,y)
-- curry f   = \ x y -> f(x,y)
-- curry     = \ f x y -> f (x,y)

-- uncurry f = \(x,y) -> f x y
-- uncurry   = \f (x,y) -> f x y


{- 

function of type  a->b->c is in curried form
function of type a*b->c is in uncurried form

curry function makes sure that a function of uncurried form can be used in curry form

-}

-- uncurried
add :: (Integer , Integer) -> Integer
add (x,y) = x+y

--curried form
addP :: Int -> Int -> Int
addP x y = x+y

curry add x y = add (x,y)

uncurry addP (x,y) = addP x y


{-
 splitOn , splitOneOf , 

>>> chunksOf 3 ['a'..'z']
["abc","def","ghi","jkl","mno","pqr","stu","vwx","yz"]

-}
words :: String -> [String]
words str = splitOn " " str

{-
dropWhile (==' ') removes the staring space from the rest otherwise infinite loop
 break (== ' ') str, the predicate is (== ' '), which checks if an element is a space character. Therefore, break (== ' ') str splits the input string str into two parts:

The first part, represented by the variable word, contains characters from the beginning of the string str until the first space character is encountered.
The second part, represented by the variable rest, contains the remaining characters of the string str after the first space character (including the space character itself).
-}
splitIntoWords "" = []
splitIntoWords str = let (word,rest) = break(== ' ')str in word:splitIntoWords(dropWhile (==' ')rest)

wordLength :: String -> Int
length :: [a] -> Int
length [] = 0
length (x:xs) = 1+Main.length xs


wordLength str = Main.length (Main.words str)l

fst :: (Int,Int) -> Int
fst (a,b) = a 

snd :: (Int, Int) -> Int
snd (a,b) = b 

sum :: [Int] -> Int

sum [] = 0
sum (x:xs) = x + Main.sum xs

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

take :: Int -> [a] -> [a]

take n [] = []
take n (x:xs) = if n>0 then x : Main.take (n-1) xs
                else []

map :: (a->b) -> [a] -> [b]

mul y = y*5

map f [] = []
map f (x:xs) = (f x) : Main.map f xs

foldl :: (s->a->s)->s->[a]->s
foldr :: (a->s->s)->s->[a]->s

foldr f s [] = s
foldr f s (x:xs) = f x (Main.foldr f s xs)


-- foldl f s (x:xs)  = Main.foldl f (f s x) xs 

reverse :: [a] -> [a]

reverse [] = []
reverse (x:xs) = (Main.reverse xs) ++ [x]

foldl f s [] = s
foldl f s xs = Main.foldr (\x acc -> acc `f` x) s (Main.reverse xs)

-- now type is s->a->s
re = \x y -> y:x 

ones = 1:ones

---------------------------------------------------------------

  -- words   :: String -> [String]

  -- unwords ["x", "y", "z"]
  --    "x y z"
