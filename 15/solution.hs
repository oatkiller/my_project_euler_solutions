main = putStrLn $ show $ iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1] !! 40 !! 20
