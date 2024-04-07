module Parser where 


import Data.Char
-- Declaring new type 
-- Parser parses a from String ...and then parses the rest?
-- Parsing may or maynot be successful so ...

newtype Parser a = Parser (String -> Result a )

-- digit :: Parser Char
digit  = satisfy isDigit
alpha = satisfy isAlpha

char :: Char -> Parser Char
char  x = satisfy (\c-> c == x) 

satisfy ::  (Char -> Bool) -> Parser Char

-- pr :: Char->Bool
satisfy pr = Parser fn
            where 
                -- fn :: (String -> Maybe(Char,String))
                fn (x:xs) = if pr x then (Ok x xs)
                            else Err
                fn [] = Err

-- Alternative (<|>)
-- (<|>) :: Parser a -> Parser a -> Parser a 
-- try left parser -> pass -> stop , then right 

-- type Result a = Maybe ( a , String )

runParser :: Parser a -> String ->  Result a 
runParser (Parser fn) = fn
instance Alternative Parser where 
    empty = Parser $ const Err 
    p1 <|> p2 = Parser fn
                where fn inp = case runParser p1 inp of 
                                    Err -> runParser p2 inp
                                    x -> x 

many :: Parser a -> Parser [a]

-- many p = Parser fn 
--         where fn inp = case runParser p inp of 
--                         Err -> Ok [] inp
--                         Ok x rest -> case runParser (many p) rest of
--                             Ok xs rest' -> Ok (x:xs) rest'
--                             Err -> Ok [x] rest 

many1 :: Parser a -> Parser [a] 
data Result a = Ok a String
            | Err 

instance Functor Result where 
    fmap f (Ok a s) = Ok ( f a) s
    fmap f Err = Err 


instance Functor Parser where 
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap f pa = Parser fn
                where 
                    -- fn :: String -> Maybe(b,String)
                    fn inp = fmap f (runParser pa inp)

instance Applicative Parser where 
    -- (<*>) :: Parser (a->b) -> Parser a -> Parser b
    -- pure :: a -> Parser a 
    pure a = Parser pfn
                where   
                    -- pfn :: String -> Result a 
                    pfn inp = Ok a inp

    (<*>) pf pa = Parser fn 
                where 
                    fn inp = case runParser pf inp of 
                                Ok f rest -> fmap f (runParser pa rest)
                                Err -> Err



    -- pf :: Parser(a->b)
    -- pa :: Parser a 

data Person  = Person String Int 

name = many1 alpha
age :: Parser [Int]
age = fmap read (many1 digit)

many1 p = (:) <$> p <*> many p
many p = many1 p <|> pure []

_repeat :: Monad m => Int -> m a -> m [a]
_repeat n action = if n <= 0 then pure []
                    else (:) <$> action <*> (_repeat (n-1) action)


instance Monad Parser where 
    (>>=) pa pbf = Parser fn 
                    where fn inp = case runParser pa inp of 
                                    Err -> Err
                                    Ok x str ->runParser (pbf x) str
