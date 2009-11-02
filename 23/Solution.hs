import Data.List as List
import Data.Set as Set

main = putStrLn $ show $ solution
	where
		solution = sum (Set.toList numbers_that_are_not_the_sum_of_two_abundants)
		numbers_that_are_not_the_sum_of_two_abundants = y_not_in_xs (sum_numbers Set.empty small_abundants) (Set.fromList [1..abundant_boundary])
			where
				abundant_boundary = 28123
				small_abundants = List.filter abundant [1..abundant_boundary]
					where
						abundant n = (sum $ divisors n) > n
							where
								divisors =
									let 
										d n = List.filter ((==0) . mod n) [2 .. n `div` 2]
									in ((List.map d [0..]) !!)
						small_abundants = List.filter abundant [1..abundant_boundary]
				sum_numbers ax [] = ax
				sum_numbers ax xxs@(x:xs) = sum_numbers set_union xs
					where
						set_union = Set.union ax new_set
						new_set = Set.fromList new_list
						new_list = [x + s | s <- xxs]
				y_not_in_xs xs ys = Set.filter (\s -> not (Set.member s xs) ) ys
