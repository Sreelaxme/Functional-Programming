module Expr where 

import Control.Applicative

data Expr =     C Double 
            |    
            |   Plus Expr Expr
            |   Minus Expr Expr
            |   Mul Expr Expr
            |   Div Expr Expr

eval :: Expr -> Maybe Double

eval (C c) = Just c 
eval (UMinus e) = fmap negate $ eval e
eval (Plus e1 e2) = (+) <$> eval e1 <*> eval e2   
eval (Minus e1 e2) = (-) <$> eval e1 <*> eval e2   
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = case eval e2 of 
                    Nothing -> Nothing
                    Just 0 -> Nothing
                    Just v -> (/v) <$> eval e1 

                
