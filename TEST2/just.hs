-- data Maybe a = Just a | Nothing

-- data Either a b = Left a | Right b

concat :: String -> String
concat a = a ++ a 
either = 
        let s = Left "foo" :: Either String Int
            n = Right 3 :: Either String Int
        in fmap (*2) s


