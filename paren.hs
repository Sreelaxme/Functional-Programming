import Data.Char 

data Result a = Ok a String 
                | Err 
                deriving Show
            
instance Functor Result where 
    -- fmap :: (a->b) -> Result a -> Result b
    fmap f (Ok x str) = Ok (f x) str 
    fmap f Err = Err 
         
newtype Parser a = Parser (String -> Result a)

runParser :: Parser a -> String -> Result a 
runParser (Parser fn ) = fn 

instance Functor Parser where 
    --fmap :: (a->b) -> Parser a -> Parser b
    fmap f pa = Parser fn
                -- fn:: String -> Maybe(b ,String)
                where fn inp = fmap f (runParser pa inp)


instance Applicative Parser where 
    -- <*> :: Parser(a->b) -> Parser a -> Parser b 
    -- pure :: a -> Parser a 
    pure x = Parser fn
            where fn inp = Ok x inp
    (<*>) pf pa = Parser fn
                where -- fn :: String -> Result b 
                fn inp = case runParser pf inp of 
                            Ok f rest -> fmap f (runParser pa rest)
                            
                            Err -> Err 


instance Monad Parser where 
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) pa pbf = Parser fb 
                where fb input = case runParser pa input of 
                                    Err -> Err 
                                    Ok x rest -> runParser (pbf x) rest 
                                

satisfy :: (Char-> Bool) -> Parser Char
satisfy f = Parser fn
            where 
                fn (x:xs) = if f x then Ok x xs
                                else Err 
                fn []     = Err 

char :: Char -> Parser Char 
char x = satisfy (==x)

digit = satisfy isDigit
alpha = satisfy isAlpha

p1 <|> p2 = Parser fn 
            where fn inp = case runParser p1 inp of
                            Err -> runParser p2 inp
                            x -> x 

many :: Parser a -> Parser [a]
many p = many1 p <|> pure [] 
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p 

integer :: Parser Int 
integer = fmap read (many1 digit)

string :: Parser [Char]
string = many1 alpha 

checkLeft :: Parser Char
checkLeft = char '('


checkRight :: Parser Char
checkRight = char ')'

parens :: Parser a -> Parser a 
            
parens p = do
    checkLeft
    result <- p
    checkRight
    return result