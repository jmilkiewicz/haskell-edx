romanToArabic  = romanToArbic 0
romanToArbic _ [] = 0
romanToArbic p (x:xs) 
	| x == 'M' = if p == 100 then 800  +  romanToArbic 1000 xs else 1000  + romanToArbic 1000 xs
	| x == 'D' = if p == 100 then 300  +  romanToArbic 500 xs else  500  + romanToArbic 500 xs
	| x == 'C' = if p == 10 then 80  +  romanToArbic 100 xs else  100  + romanToArbic 100 xs
	| x == 'L' = if p == 10 then 30  +  romanToArbic 50 xs else  50  + romanToArbic 50 xs	
	| x == 'X' = calc p 1 10 + romanToArbic 10 xs
	| x == 'V' = if p == 1 then 3  +  romanToArbic 5 xs else  5  + romanToArbic 5 xs
	| x == 'I' = 1 +  romanToArbic 1 xs


calc p g v 
	| p == g = v - (2 * g) 
	| otherwise = v


