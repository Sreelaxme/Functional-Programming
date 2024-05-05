module Rule where 
    
import Parser 

data S = RuleA Char S Char
        | RuleB Char S Char
        | RuleC 
        -- deriving Show

-- parseRuleA = RuleA <$> char 'a' <*> parseRuleA <*> char 'a'
-- parseRuleB= RuleB <$> char 'b' <*> parseRuleB <*> char 'b'
-- parseRuleC = pure RuleC

sParse :: Parser S
sParse = parseRuleA <|> parseRuleB <|> parseRuleC
  where
    parseRuleA = RuleA <$> char 'a' <*> sParse <*> char 'a'
    parseRuleB = RuleB <$> char 'b' <*> sParse <*> char 'b'
    parseRuleC = pure RuleC

instance Show a => Show (Result a) where
  show (Ok x _) = "Ok " ++ show x
  show Err = "Err"

instance Show S where
  show (RuleA c1 s c2) = "RuleA " ++ [c1] ++ " (" ++ show s ++ ") " ++ [c2]
  show (RuleB c1 s c2) = "RuleB " ++ [c1] ++ " (" ++ show s ++ ") " ++ [c2]
  show RuleC = "RuleC"
-- Define a function to test the parser
testParser :: String -> IO ()
testParser input = case runParser sParse input of
                     Ok result _ -> putStrLn $ "Parsed result: " ++ show result
                     Err -> putStrLn "Parsing failed"

-- Define the main function to test the parser
main :: IO ()
main = do
  testParser "aXb"
  testParser "cYd"
