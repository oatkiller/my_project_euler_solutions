import Data.List
binary x = unfoldr (\x -> if x == 0 then Nothing else Just (head $ show r,q)) x
	where
		(q,r) = quotRem x 2
