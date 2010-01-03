hexagonal_numbers = [ n * (2 * n - 1) | n <- [1..]]
integer x = fromInteger (truncate x) == x
pentagonal x = integer ((sqrt (24 * x + 1) + 1) / 6)
solution = [ x | x <- hexagonal_numbers, pentagonal $ fromInteger x ] !! 2
main = putStrLn $ show $ solution
