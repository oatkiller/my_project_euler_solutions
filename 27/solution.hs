import Data.Function
import Data.List

{-- strict maximumBy --}
maximumBy'               :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ []          =  error "List.maximumBy: empty list"
maximumBy' cmp xs        =  foldl1' maxBy xs
	where
		maxBy x y = case cmp x y of
			GT -> x
			_  -> y


{-- prime crap --}

--primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

trialDivision ps n = doTrialDivision ps
	where doTrialDivision (p:ps) = let (q,r) = n `quotRem` p in if r == 0 then False else if q < p then True else doTrialDivision ps
	      doTrialDivision [] = True
-- q < p => p > sqrt n

primesTo10000 = primesTo100 ++ filter (trialDivision primesTo100) [101,103..9999]

isTrialDivisionPrime 2 = True -- special case, not caught by above code
isTrialDivisionPrime n = trialDivision (primesTo10000 ++ [10001,10003..]) n

is_prime = (map isTrialDivisionPrime [0..] !!)

{-- actual problem --}

solve_quadratic n a b = n_squared + an + b
	where
		n_squared = n * n
		an = a * n

--is_quadratic_prime :: Integer -> Integer -> Integer -> Bool
is_quadratic_prime n a b = if solution > -1 then is_prime solution else False
	where
		solution = solve_quadratic n a b

product_of_coefficients a b = a * b

get_number_of_primes_for_consecutive_values n a b = if quadratic_is_prime then 1 + get_number_of_primes_for_consecutive_values m a b else 0
	where
		quadratic_is_prime = is_quadratic_prime n a b
		m = succ n

limit = 1000
lower_limit = 0 - limit
ab = [(a,b) | a <- [lower_limit..limit], b <- [lower_limit..limit]]

get_number_of_primes_for_consecutive_values_starting_with_zero = get_number_of_primes_for_consecutive_values 0

number_of_primes_for_consecutive_values = map (\(a,b) -> get_number_of_primes_for_consecutive_values_starting_with_zero a b) ab 

primes_ab = zip number_of_primes_for_consecutive_values ab

best_ab = snd $ maximumBy' (compare `on` fst) primes_ab

solution = product_of_coefficients a b
	where
		(a,b) = best_ab

main = putStrLn $ show $ solution
