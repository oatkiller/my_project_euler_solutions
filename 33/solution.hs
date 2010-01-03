import Data.List
import Data.Char
{--
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
		fraction_without_digit_is_same
			

have_same_digit :: [Int] -> [Int] -> Bool
have_same_digit as bs = length (intersect as bs) > 0
--}

--fractions = [(a,b) | a <- [10..98], b <- [11..99], b > a, have_common_digit a b, numbers_have_no_zero a b, curious_fraction a b]
fractions = [(a,b) | a <- [10..98], b <- [11..99], b > a, have_common_digit a b, numbers_have_no_zero a b, curious_fraction (fromInteger a) (fromInteger b)]
have_common_digit a b = length (number_intersect a b) > 0
number_intersect a b = intersect (show a) (show b)
number_union a b = union (show a) (show b)
numbers_have_no_zero a b = not (elem '0' (number_union a b))
improperly_reduce a b = (a_without_intersection,b_without_intersection)
	where
		intersection = number_intersect a b
		subtract_intersection x = read (x \\ intersection) :: Integer
		a_without_intersection = subtract_intersection $ show a
		b_without_intersection = subtract_intersection $ show b
curious_fraction :: Float -> Float -> Bool
curious_fraction a b = a / b * (fromInteger bottom) == (fromInteger top)
	where
		(top,bottom) = improperly_reduce a b
