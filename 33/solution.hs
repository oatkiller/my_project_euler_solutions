import Data.List
import Data.Char
import Data.Ratio

fractions = [(a,b) | a <- [10..98], b <- [11..99], b > a, have_common_digit a b, numbers_have_no_zero a b, curious_fraction (fromInteger a) (fromInteger b)]
have_common_digit a b = length (number_intersect a b) > 0
number_intersect a b = intersect (show a) (show b)
number_union a b = union (show a) (show b)
numbers_have_no_zero a b = not (elem '0' (number_union a b))
improperly_reduce a b
	| length intersection == 1 = (a_without_intersection,b_without_intersection)
	| otherwise = (0,1)
	where
		intersection = number_intersect a b
		subtract_intersection x = read (x \\ intersection) :: Integer
		a_without_intersection = subtract_intersection $ show a
		b_without_intersection = subtract_intersection $ show b
curious_fraction :: Float -> Float -> Bool
curious_fraction a b = a / b * (fromInteger bottom) == (fromInteger top)
	where
		(top,bottom) = improperly_reduce (floor a) (floor b)
solution = let (a,b) = (product $ map fst fractions,product $ map snd fractions) in denominator $ a % b
