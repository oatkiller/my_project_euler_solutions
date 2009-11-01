import Data.List

solution = length (filter (\(d,i) -> i `mod` 7 == day_index && d == 1) indexed_dates)
	where
		day_index = 6 -- sunday is the 7th day when counting from monday
		indexed_dates = zip dates [1..]
		dates = concat $ concat $ map calendar_dates [1901..2000]
			where
				calendar_dates year = [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec]
					where
						(sep:apr:jun:nov:_) = have 30
						(jan:mar:may:jul:aug:oct:dec:_) = have 31
						feb
							| leap = [1..29]
							| otherwise = [1..28]
							where
								leap
									| year `mod` 100 == 0 = if year `mod` 400 == 0 then True else False
									| year `mod` 4 == 0 = True
									| otherwise = False
						have count = repeat (take count [1..])
