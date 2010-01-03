import Data.List
import Data.Char
binary x = map intToDigit $ reverse $ unfoldr (\x -> if x == 0 then Nothing else Just $ remQuot x 2) x
	where
		remQuot x y = let (a,b) = quotRem x y in (b,a)
palindrome x = x == reverse x
palindromic_in_both_bases x = palindrome bx && palindrome dx
	where
		bx = binary x
		dx = show x
solution = sum $ filter palindromic_in_both_bases [1,3..999999]
main = putStrLn $ show $ solution
