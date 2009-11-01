import Data.Char
solution = sum $ map digitToInt answer
	where
		answer = show $ fac 100
			where
				fac n = if n == 0 then 1 else n * fac (n-1)
