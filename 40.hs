import Data.Char

solution = product $ map d (map (10 ^) [0..6])
  where
    d n@(i + 1) = digits n 1
    digits n@(i + 1) x
      | lns > i = digitToInt (ns !! i)
      | otherwise = digits (n - lns) (succ x)
      where
        ns = show x
        lns = length ns