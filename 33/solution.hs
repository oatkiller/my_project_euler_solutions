import Data.List
import Data.Char
{--
fractions = [(a,b) | a <- [10..98], b <- [11..99]]
non_trivial_fractions = filter (\(a,b) -> have_same_non_gcd_number a b) fractions
	where
		have_same_number a b = length (intersect a b) > 0
		have_same_non_gcd_number a b = have_same_number (except_gcd a) (except_gcd b)
			where
				g = gcd a b
				except_gcd x = delete g xs
					where
						xs = show x

--}

non_trivial_fractions = filter (\(a,b) -> notcoprime a b && gcd_is_one_digit a b && have_same_non_gcd_digit a b) fractions
	where
		fractions = [(a,b) | a <- [10..98], b <- [11..99], b > a]
		notcoprime :: Int -> Int -> Bool
		notcoprime a b = gcd a b /= 1
		gcd_is_one_digit :: Int -> Int -> Bool
		gcd_is_one_digit a b = length (show g) == 1
			where 
				g = gcd a b
		have_same_non_gcd_digit :: Int -> Int -> Bool
		have_same_non_gcd_digit a b = have_same_digit as bs
			where
				g = gcd a b
				as = delete g (map digitToInt (show a))
				bs = delete g (map digitToInt (show b))

have_same_digit :: [Int] -> [Int] -> Bool
have_same_digit as bs = length (intersect as bs) > 0
