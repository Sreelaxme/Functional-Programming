import Control.Applicative (Alternative(empty,(<|>)))
import Data.Char (isAlpha)

import Parser

data S = RuleA Char S Char
        | RuleB Char S Char
        | RuleE

string :: String -> Parser String
string [] = pure []
string (x:xs) = (:) <$> char x <*> string xs 

ruleA :: Parser S 
ruleA = RuleA <$> alpha <*> ruleA <*> alpha
ruleB :: Parser S
ruleB = RuleB <$> alpha <*> ruleB <*> alpha
ruleE :: Parser S 
ruleE = pure RuleE 

startSymbol :: Parser S 
startSymbol = ruleA <|> ruleB <|> ruleE 
