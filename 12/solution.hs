--triangle = ((map triangle' [0 ..]) !!)
	--where
		--triangle' 1 = 1
		--triangle' n = triangle' (n-1) + n
--
--solution 1 = 1
--solution 2 = 2 + (solution (2 - 1))

--triangle n = n*(n+1)`div`2

int_sqrt :: Integer -> Integer
int_sqrt n = floor (sqrt (fromInteger n))

factor :: Integer -> Integer
factor n = n
