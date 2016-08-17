
leftpad c l xs 
	|length xs >= l = xs
	|otherwise = leftpad c l (c:xs)  