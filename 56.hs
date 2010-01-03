import Data.Char
solution = maximum digital_sums
	where
		digital_sums = map sum digits
		digits = map (map digitToInt) numbers
		numbers = map show [a^b | a <- [1..100], b <- [1..100]]
