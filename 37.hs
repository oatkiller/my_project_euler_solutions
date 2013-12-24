import Math.NumberTheory.Prime (primes, isPrime)
import Data.Digits
import Data.List

main = putStrLn $ show $ sum $ take 11 truncatable_primes
	where
		truncatable_primes = [prime | prime <- primes, isTruncatable prime]
			where
				isTruncatable n
					| n <= 10 = False
					| otherwise = all isPrime (numbersToTest n)
				numbersToTest n = (foldl (++) [] list_of_permutations) ++ [n]
					where
						list_of_permutations = map (permutation digits_of_n) [1..pred $ length digits_of_n]
						digits_of_n = digits 10 n
						permutation [] _ = []
						permutation xs@(x:[]) _ = xs
						permutation xs n = [firstNumber,secondNumber]
							where
								firstNumber = unDigits 10 (inits xs !! n)
								secondNumber = unDigits 10 (reverse (tails xs) !! n)
