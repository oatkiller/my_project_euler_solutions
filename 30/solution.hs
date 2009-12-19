import Data.Char
sum_of_powers_of_digits :: Int -> Int -> Int
sum_of_powers_of_digits p n = sum powers
	where
		characters = show n
		numbers = map digitToInt characters
		powers = map (^p) numbers

auspicious 1 = False
auspicious n = sum_of_powers_of_digits 5 n == n
solution = sum $ filter auspicious [0..999999]
main = putStrLn $ show solution
