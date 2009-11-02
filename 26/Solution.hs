import Data.List (elemIndex)
import Maybe

-- read about Full Reptend Prime

main = print (solution 1 999 1 0)
	where
		solution current limit highest max_so_far
			| current == limit = highest
			| max_so_far > cycle_length = solution d limit highest max_so_far
			| cycle_length >= max_so_far = solution d limit current cycle_length
			where
				d = succ current
				cycle_length = cycle_length' [] 1 current
					where
						cycle_length' :: [Int] -> Int -> Int -> Int
						cycle_length' px n d
							| elem n px = let mi = elemIndex n px; i = fromMaybe 0 mi in length (drop i px)
							| n < d = cycle_length' npx (n * 10) d
							| r /= 0 = cycle_length' npx (r * 10) d
							| otherwise = 0
							where
								t = n `div` d
								r = n `mod` d
								npx = px ++ [n]
