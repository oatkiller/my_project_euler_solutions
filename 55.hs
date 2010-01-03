palindrome n = ns == reverse ns
	where
		ns = show n
reverse_and_add n = n + m
	where
		m = read (reverse $ show n) :: Integer
lychrel_tries 0 n = True
lychrel_tries tries n = if palindrome m then False else lychrel_tries (pred tries) m
	where
		m = reverse_and_add n
lychrel = lychrel_tries 50
solution = length $ filter lychrel [0..10000]
