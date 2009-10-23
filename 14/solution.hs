import Data.List

main = putStrLn $ show $ maximum' $ map (sequence_length) [1..999999]
	where 
		maximum' = foldl1' $ (\x y -> if snd x > snd y then x else y)
		sequence_length n = (n,length $ collatz n)
			where
				collatz = unfoldr step
					where
						step 0 = Nothing
						step 1 = Just (1, 0)
						step x = if odd x
							then Just (x, 3 * x + 1)
							else Just (x, x `div` 2)
