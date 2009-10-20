is_palindromic n = shown == reverse shown
	where shown = show n
three_digit_numbers = [999,998..1]
solution = maximum [ x * y | x <- three_digit_numbers, y <- three_digit_numbers, is_palindromic(x * y) ]
