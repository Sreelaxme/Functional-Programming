-- import Data.List.Split
-- _unwords :: [String] -> String

-- _unwords (x:xs) = x ++ _unwords xs  
-- -------------------------------------------------
-- _lines  :: String -> [String]
-- -- _lines str = let (line,rest) = break(== '.') str in line:_lines(dropWhile (== '.') rest)

-- _lines str = splitOn "." str

-- _unlines :: [String] -> String

-- _unlines (x:xs) = x ++ _unlines xs

_min [] = (1000,0)
_min (x:xs) = let 
                (p,i) = _min xs 
            in
                if x < p then (x, i+1)
                else (p , i+1)

_create :: (Int,Int) ->[Int]
_create (minN,0) = []
_create (minN,num) = minN : _create (minN,(num-1))

-- tl [] = []
-- tl (x:xs) = xs


-- tri = 0:1:2:seq
--         where 
--             seq = zipWith (\x y z -> x+y+z) tri (tl tri) (tl (tl tri))