import Data.List
contain_same_digits n m = sort (show n) == sort (show m)
solution = head $ filter auspicious [1..]
	where
		auspicious x = all (contain_same_digits x) (map (*x) [2..6])
