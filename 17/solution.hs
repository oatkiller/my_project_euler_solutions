import Data.List
digits = words "one two three four five six seven eight nine"
tens = words "ten twenty thirty forty fifty sixty seventy eighty ninety"
teens = words "eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

word n
	| n == 0 = ""
	| n == 1000 = spaces ["one","thousand"]
	| n > 99 && n `mod` 100 == 0 = spaces [word (n `div` 100),"hundred"]
	| n > 99 = spaces [word (n - l),"and",word (n `mod` 100)]
	| n < 10 = digits !! i
	| n > 10 && n < 20 = teens !! (i - 10)
	| n > 9 && n < 100 && n `mod` 10 == 0 = tens !! (pred $ n `div` 10)
	| n >= 20 = spaces [word (n - m), word m]
	where
		i = pred n
		m = n `mod` 10
		l = n `mod` 100
		spaces = concat . intersperse ""

main = putStrLn $ show $ length $ concat $ map word [1..1000]
