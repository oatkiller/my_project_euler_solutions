import Data.List
values = [1,2,5,10,20,50,100,200]
combinations :: [Int] -> Int -> Int
combinations (coin:[]) 0 = 1
combinations (coin:[]) x = if x `mod` coin == 0 then 1 else 0 
combinations (coin:remaining_coins) 0 = 1
combinations (coin:unsorted_remaining_coins) value = (sum $ map (combinations remaining_coins) remaining_values)
	where
		remaining_coins = reverse $ sort unsorted_remaining_coins
		remaining_values = [ value - (x * coin) | x <- [0..value `div` coin] ]
main = putStrLn $ show $ combinations values 200
