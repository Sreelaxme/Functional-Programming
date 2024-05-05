module Int where 

import Parser

integer :: Parser Int
integer = read <$> many digit
