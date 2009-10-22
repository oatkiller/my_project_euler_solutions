import Data.List

collatzSequenceLength n = (n,length $ collatzSequence n)
	where 
		collatzSequence = unfoldr collatz
		collatz 0 = Nothing
		collatz 1 = Just (1, 0)
		collatz x = if odd x
			then Just (x, 3 * x + 1)
			else Just (x, x `div` 2)

maximum' :: (Ord a) => [(n,a)] -> (n,a)
maximum' = foldl1' $ (\x y -> if snd x > snd y then x else y)

solution = maximum' $ map (collatzSequenceLength) [1..999999]
main = putStrLn $ show $ solution
