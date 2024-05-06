-- newtype Parser a = Parser(String -> a)
-- But mi8 not be successful hence String -> Maybe (a,String)
-- newtype Parser a = Parser (string -> Maybe (a,tring))
--  gonna create a new type for Maybe(a,String)
module Parser where 

import Data.Char
-- import Text.Parsec
-- newtype Result a = Maybe(a,String)
data Result a  = Ok a String
            | Err 

newtype Parser a = Parser (String -> Result a )

runParser :: Parser a -> String -> Result a 
runParser (Parser fn) = fn 

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
                            -- Ok f rest -> case runParser pa rest of 
                            --     Ok a rest2 - > Ok (f a) rest2
                            --     Err -> Err 
                            Err -> Err 


instance Monad Parser where 
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) pa pbf = Parser fb 
                where fb input = case runParser pa input of 
                                    Err -> Err 
                                    Ok x rest -> runParser (pbf x) rest 

-- Characeter parser 
satisfy :: (Char -> Bool) -> Parser Char
-- satisfy pr = Parser fn 
--             where 
--                 -- fn :: String -> Maybe(char,String)
--                 fn (x:xs) = if pr x then Just(x,xs)
--                             else Nothing 
--                 fn [] = Nothing
satisfy pr = Parser fn 
            where 
                -- fn :: String -> Maybe(char,String)
                fn (x:xs) = if pr x then Ok x xs
                            else Err
                fn [] = Err

-- digit is a digit parser
digit = satisfy isDigit
-- alphabet parser
alpha = satisfy isAlpha
char x = satisfy (\c -> c==x)


-- Alternative <|>
-- <|> :: Parser a -> Parser a -> Parser a 

p1 <|> p2 = Parser fn 
            where fn inp = case runParser p1 inp of
                            Err -> runParser p2 inp
                            x -> x 

instance (Show a) => Show (Result a) where
  show (Ok x _) = "Ok " ++ show x
  show Err = "Err"
instance Functor Result where 
    -- fmap :: (a->b) -> Result a -> Result b
    fmap f (Ok x str) = Ok (f x) str 
    fmap f Err = Err 
         
-- data 
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p


----------------------------------------------------------------------
