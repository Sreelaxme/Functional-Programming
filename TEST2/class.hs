class Equal a where 
    eq ::a -> a-> Bool

instance Equal Bool where
    eq True True    = True
    eq False False  = False
    eq _ _          = False  

instance (Equal a, Equal b) => Equal (a,b) where 
    eq (a1,b1) (a2,b2) = (eq a1 a2) && (eq b1 b2)

----------This doesn't work coz hash function is not defined-------------

-- hash :: String -> Hash

-- data HString = HString Hash String 

-- fromString :: String -> HString
-- fromString str = HString (hash str) str

-- toString :: HString -> String
-- toString (HString _ str) = str

-- instance (Equal Hash, Equal String) => Equal HString where
--     eq (HString h1 s1) (HString h2 s2) = (eq h1 h2) && (eq s1 s2)

-----------------------------------------------------------------



-- The Bounded typeclass in Haskell is used to define types that have a lower and upper bound. It provides two functions:

-- minBound :: a: Returns the minimum value of a type.
-- maxBound :: a: Returns the maximum value of a type.
--  writing deriving at the end 
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq,Enum,Bounded)
instance Equal Day where 
    eq Sun Sun = True 
    eq Mon Mon = True
    eq Tue Tue = True
    eq Wed Wed = True
    eq Thu Thu = True
    eq Fri Fri = True
    eq Sat Sat = True
    eq _ _      = False 

instance Ord Day where 
    compare x y = compare (fromEnum x) (fromEnum y)




