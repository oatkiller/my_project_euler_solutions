import Data.List
import Primes
numbers :: [Integer]
numbers = reverse $ sort $ map read (flatten $ map permutations (tail $ inits $ concatMap show [1..7]))
flatten [] = []
flatten (x:xs) = x ++ flatten xs
solution = head $ filter isMillerRabinPrime numbers
