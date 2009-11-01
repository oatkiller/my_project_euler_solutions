solution = sum $ filter amicable [3..n]
	where
		n = pred 10000

amicable = 
	let
		a n = m == n && n /= x
			where
				x = g n
				m = g x
				g = sum . divisors 
				divisors n = 1 : filter ((==0) . mod n) [2 .. n `div` 2]
	in
		((map a [0..]) !!)

main = putStrLn $ show $ solution
