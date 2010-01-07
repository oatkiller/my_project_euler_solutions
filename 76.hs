import Data.List

ways =
	let
		ways' 1 = [[1]]
		ways' 2 = [[1,1]]
		ways' x = nub $ map sort (flatten $ map (\n -> [x - n,n] : (zipWith (++) (repeat [x - n]) (ways n))) [1..pred x])
	in
		(map ways' [0..] !!)

flatten (x:[]) = x
flatten (x:xs) = x ++ flatten xs
solution = length $ ways 100
main = putStrLn $ show solution
