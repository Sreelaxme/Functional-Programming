-- Class Eq a => Ord a where
--     (<=):: a -> a -> Bool

-- instance (Ord a , Ord b) => Ord (a,b) where
--     (<=) (x1,y1) (x2,y2)    | x1<=x2 = True
--                             | x1==x2 = y1<=y2
--                             | otherwise = False

-- instance Ord a => Ord [a] where
--     (<=) x:xs y:ys = x<=y || xs<=ys

-- head :: [a] -> a
-- head x:xs = x



-- sort:: Ord a => [a] -> [a]
-- sort x:xs   | x <= (head xs) = x : sort xs
--             | otherwise 

sort :: Ord a => [a] -> [a]
sort [] = []  -- Base case: Sorting an empty list results in an empty list
sort (x:xs) = 
    let smallerSorted = sort [a | a <- xs, a <= x]  -- Sort elements smaller than or equal to 'x'
        biggerSorted = sort [a | a <- xs, a > x]    -- Sort elements bigger than 'x'
    in  smallerSorted ++ [x] ++ biggerSorted        -- Concatenate the sorted smaller elements, 'x', and the sorted bigger elements

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x] 
mergeSort (xs) = 
    let (left, right) = splitAt (length xs `div` 2) xs
        sortedLeft = mergeSort left
        sortedRight = mergeSort right
    in merge sortedLeft sortedRight


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x: merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys


print a = show a