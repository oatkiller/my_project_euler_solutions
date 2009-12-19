main = putStrLn $ reverse $ take 10 $ reverse $ show $ sum $ map (\n -> n ^ n) [1..1000]
