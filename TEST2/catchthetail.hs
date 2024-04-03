-- deviation is [x1- u. x2 - u...] where u is sum(xis)/n
_sum :: [Double] -> (Double,Double)
_sum (x:xs) = (av , n)
            where 
                (p,i) =  avg(xs)
                av = p + x
                n = i+1
_sum [] = (0,0)
avg :: [Double] -> Double
avg xs = (x/y) where
                    x = fst $ _sum xs
                    y = snd $ _sum xs
                    
deviation:: [Double] -> [Double]
deviation (x:xs) = (x - u) : (deviation xs)
                    where u = avg(x:xs)
                    
deviation [] = []

----------------------------------------
helper :: [Double] -> Double  ->Double-> ([Double],Double,Double)
helper (x:xs) ln avg = (lst , n , av)
                where 
                    (ol, on, oavg ) = helper xs ln avg
                    lst = (x - avg) : ol
                    n = on + 1
                    av = (oavg+ (x/ln))
helper [] _ _ = ([], 0.0,0.0)
onePassDeviation :: [Double] -> [Double]
onePassDeviation xs = lst
                    where 
                        (lst , ln , avg ) = helper xs ln avg