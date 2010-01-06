p_length p = (p,length [(a,b,p - a - b) | a <- [1..p / 4], b <- [succ a..(p - a) / 2], b > a, sqrt(a^2 + b^2) == p - a - b])
solution = foldr1 (\(current_p, current_length) (p,length) -> if length > current_length then (p,length) else (current_p,current_length)) [p_length p | p <- [3..1000]]
main = putStrLn $ show $ solution
