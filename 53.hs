facs = scanl (*) 1 [1..]
fac n = facs !! n
c n r = fac n `div` (fac r * (fac (n - r)))
solution = length $ filter (> 1000000) [c n r | n <- [1..100], r <- [1..100], r <= n]
