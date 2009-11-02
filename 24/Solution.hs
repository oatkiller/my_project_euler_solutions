import Data.Char (intToDigit)
import Data.List (sort, permutations)
main = putStrLn $ show $ solution
	where
		solution = p !! i
			where
				i = pred 1000000
				p = sort px
					where
						px = permutations string
							where
								string = map intToDigit [0..9]
