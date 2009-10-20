-- solution = head [ x | x <- [lower_bound..], (filter (\y -> x `mod` y == 0) set_of_divisors) == set_of_divisors ]

lower_bound = 2520
must_be_divisible_by = [11,12,13,14,15,16,17,18,19,20]
solution = head [ x | x <- [lower_bound,lower_bound+20..], filter (\y -> x `mod` y == 0) must_be_divisible_by == must_be_divisible_by ]
