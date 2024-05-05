module Person where 

import Parser 

data Person = Person String Int 
-- name parser...parses the string for name 
name = many1 alpha

-- age::Parser Int
-- read :: Read a => String -> a 
age = fmap read (many1 digit)

pP :: Parser Person
pP = Person <$> (name) <*> age


-- Define a function to test the parser
testParser :: String -> IO ()
testParser input = case runParser pP input of
                     Ok (Person name age) _ -> putStrLn $ "Parsed person: " ++ name ++ ", Age: " ++ show age
                     Err -> putStrLn "Parsing failed"


instance Show Person where 
    show (Person name age) = "Name" ++ name
-- Define the main function to test the parser
main :: IO ()
main = do
  testParser "John30"
  testParser "Alice25"