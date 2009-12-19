import Data.List
import Data.Char

digits = [1..9]

two_three_product_from_list :: [Int] -> Int
two_three_product_from_list (a:b:x:y:z:[]) = f * g
	where
		f = (a * 10) + b
		g = (x * 100) + (y * 10) + z

one_four_product_from_list :: [Int] -> Int
one_four_product_from_list (a:w:x:y:z:[]) = f * g
	where
		f = a
		g = (w * 1000) + (x * 100) + (y * 10) + z

pandigital :: [Int] -> Bool
pandigital xs = (sort xs) == digits

pandigital_identity :: ([Int] -> Int) -> [Int] -> Bool
pandigital_identity product_from_list list = pandigital (list ++ ns)
	where
		p = product_from_list list
		ps = show p
		ns = map digitToInt ps

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

variables xs ns
	| length xs == 5 = [xs]
	| otherwise = flatten (map (\(x,s) -> variables x s) xss)
		where
			xss = map (\n -> (xs ++ [n],delete n ns)) ns

products product_from_list = map (\xs -> if pandigital_identity product_from_list xs then product_from_list xs else 0) (variables [] digits)

solution = sum $ nub $ flatten $ (map products [two_three_product_from_list,one_four_product_from_list])
