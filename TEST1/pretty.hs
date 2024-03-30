_zipWith :: (a->b->c) -> [a] -> [b] -> [c]

_zipWith f (x:xs) (y:ys) = f x y : _zipWith f  xs ys
_zipWith _ _ _ = []

type Row = [String]
type Table = [[String]]
zeros = 0:zeros

duplicate string n = concat $ replicate n string 
padTable ::[Int] -> Table->Table
padTable padding  = map (padRows padding) 

padRows :: [Int] -> Row -> Row
padRows  = _zipWith align 

align :: Int -> String -> String
align n s = s++ duplicate " " (n - length s)

findRowPadding :: Row -> [Int]
findRowPadding r = map length r ++ zeros

findPadding :: Table -> [Int]
findPadding = foldr combf zeros

combf :: Row -> [Int] -> [Int]
combf = _zipWith max (findRowPadding r)

------1 pass
-- helper :: [Int]->Table->(Table,[Int])
-- helper (p:ps) table = (pTable,padding)
--                 where 
--                     pTable = 

-- alignTable table = paddedTable
--         where 
--             (paddedTable,pad) = helper pad table