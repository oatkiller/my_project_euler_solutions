import Data.List

primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
	where 
		sieve (p:ps) xs = h ++ sieve ps [x | x<-t, x `rem` p /= 0]  -- or:  filter ((/=0).(`rem`p)) t
			where (h,~(_:t)) = span (< p*p) xs

isPrime n = n > 1 && n == head (primeFactors n)
 
primeFactors 1 = []
primeFactors n = go n primes
	where
		go n ps@(p:pt)
			| p*p > n        = [n]
			| n `rem` p == 0 = p : go (n `quot` p) ps
			| otherwise      = go n pt

rotate (x:xs) = xs ++ [x]
rotations xs = scanl (\x _ -> rotate x) xs xs

circular_prime n = all isPrime rx
	where
		rx = map r (rotations (show n))
			where
				r x = read x :: Integer

solution = length $ filter circular_prime [1..1000000]

main = putStrLn $ show $ solution
