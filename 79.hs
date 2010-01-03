import Data.List
import Data.Char
entries = map (\x -> map digitToInt (show x)) (sort $ nub [319,680,180,690,129,620,762,689,762,318,368,710,720,710,629,168,160,689,716,731,736,729,316,729,729,710,769,290,719,680,318,389,162,289,162,718,729,319,790,680,890,362,319,760,316,729,380,319,728,716])

supplant xs ys = if supplants xs ys then before ++ middle ++ after else ys
	where
		middle = init $ tail xs
		(before,after) = span (/= (last xs)) ys

supplants xs ys = isInfixOf [head xs,last xs] ys
supplantable xs xss = any (supplants xs) xss

reduce [] ds = furtherReduce ds []
reduce s@(xs:xss) ds = if (supplantable xs xss) then reduce ((map (supplant xs) xss) ++ ds) [] else reduce xss (ds ++ [xs])

furtherReduce [] ds = ds
furtherReduce (xs:xss) ds = if anySetsEclipseXs then furtherReduce xss ds else furtherReduce xss (ds ++ [xs])
	where
		anySetsEclipseXs = any (\ys -> all (\x -> elem x ys) xs) xss

solution = foldl (++) "" (map show (head $ reduce entries []))
