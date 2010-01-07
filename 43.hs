import Data.List
auspicious x = all (\(i,j,k,d) -> divisible_by d (substring i j k x)) cases
cases = [(8,9,10,17),(7,8,9,13),(6,7,8,11),(2,3,4,2),(3,4,5,3),(4,5,6,5),(5,6,7,7)]
substring (i+1) (j+1) (k+1) x = read [string !! i,string !! j,string !! k]
	where
		string = show x
divisible_by x a = a `mod` x == 0
numbers :: [Integer]
numbers = map read (filter (\ps@(p:_) -> p /= '0') (permutations ['0'..'9']))
pandigital xs = union xs ['0'..'9'] == xs
auspicious_numbers = filter auspicious numbers
solution = sum $ auspicious_numbers
main = putStrLn $ show $ solution
