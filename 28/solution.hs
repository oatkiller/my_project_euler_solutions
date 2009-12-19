ring_area r = r * r
perimeter_length 1 = 1
perimeter_length n = ring_area n - ring_area (n - 2)

starting_value 1 = 1
starting_value n = starting_value m + perimeter_length m
	where
		m = n - 2

sum_of_diagonals 1 = 1
sum_of_diagonals n = se + sw + nw + ne + (sum_of_diagonals (n - 2))
	where
		p = perimeter_length n
		q = p `div` 4
		se = (starting_value n) + (q - 1)
		sw = se + q
		nw = sw + q
		ne = nw + q
