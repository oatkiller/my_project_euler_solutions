import Data.List (find)

fib :: Int -> Integer
fib =
	let 
		f 0 = 0
		f 1 = 1
		f n = fib (n-2) + fib (n-1)
	in  
		(map f [0 ..] !!)

main = putStrLn $ show $ solution
	where
		solution = first_fib_with_n_digits 1000
			where
				first_fib_with_n_digits n = g 0
					where 
						g o = if fib_has_n_digits o then o else g (succ o)
							where
								fib_has_n_digits x = (length $ fib_string x) >= n
									where
										fib_string x = show $ fib x
