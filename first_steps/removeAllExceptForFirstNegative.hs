ff [] = []
ff (x:xs)
	|x >= 0 = ff (xs)
	|otherwise = [x]