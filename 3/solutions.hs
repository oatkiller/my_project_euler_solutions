import Data.List
integral_sqrt n = floor (sqrt (fromInteger n))
factors n = [ x | x <- [integral_sqrt n, (integral_sqrt n) - 1..2], n `mod` x == 0]
prime_factors n = [ x | x <- factors n, length (intersect (factors x) (factors n)) == 0]
greatest_prime_factor n = head (prime_factors n)
my_greatest_prime_factor = greatest_prime_factor 600851475143
