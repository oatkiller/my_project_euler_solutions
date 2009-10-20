import Data.List
my_number = 2000000

integral_sqrt n = floor (sqrt (fromInteger n))

sieve 0 = []
sieve 1 = []
sieve 2 = [2]
sieve 3 = (sieve 2) ++ [3]
sieve 4 = sieve 3
sieve 5 = (sieve 3) ++ [5]

sieve n = sieve' ([2],(filter odd [3..integral_sqrt_n]),(filter odd [(integral_sqrt_n + 1)..n])) -- map to the recursive part. use [2] for the list of primes. because of this, we 'odd' the two lists. populate the lower_list with odds between 3 and the sqrt of n. populate upper_list with odds between sqrt n .. n
	where integral_sqrt_n = integral_sqrt n
sieve' (primes,[],upper_list) = primes ++ upper_list -- done! return the primes plus everything left in the upper list
sieve' (primes,p:lower_list,upper_list) = sieve'(primes ++ [p],lower_list `except_multiples_of` p, upper_list `except_multiples_of` p)
	where except_multiples_of list n = filter (\x -> x `mod` n /= 0) list
