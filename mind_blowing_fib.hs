fib = 1 : 1 : fib' (fib)
	where
		fib' (x : y : rest) = x + y : fib' (y : rest) 


-- another one
fibs = 1 : 1 : zipWith (+) fibs (tail fibs) 
