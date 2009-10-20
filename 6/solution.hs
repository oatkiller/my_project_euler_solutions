sum_of_squares xs = ((foldr (+) 0 xs)^2)
square_of_sums xs = (foldr (\n sum -> sum + (n^2)) 0 xs)
find_answer xs = (sum_of_squares xs) - (square_of_sums xs)
