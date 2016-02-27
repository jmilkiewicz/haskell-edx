arabicToRoman n 
	| n >= 1000 = "M" ++ arabicToRoman (n - 1000)
	| n >= 500 = "D" ++ arabicToRoman (n - 500)
	| n >= 400 = "CD" ++ arabicToRoman (n - 400)
	| n >= 100 = "C" ++ arabicToRoman (n - 100)
	| n >= 50 = "L" ++ arabicToRoman (n - 50)
	| n >= 40 = "XL" ++ arabicToRoman (n - 40)
	| n >= 10 = "X" ++ arabicToRoman (n - 10)
	| n >= 9 = "IX" ++ arabicToRoman (n - 9)
	| n >= 5 = "V" ++ arabicToRoman (n - 5)
	| n == 4 = "IV" ++ arabicToRoman (n - 4)
	| n >=1 = "I" ++ arabicToRoman (n - 1)
	| otherwise = ""
