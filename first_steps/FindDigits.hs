-- https://www.hackerrank.com/challenges/find-digits
findDigits n = length [ x| x <- (d2l n),  x>0 && n `mod` x == 0]

d2l 0 = []
d2l n = d2l (n `div` 10) ++ [n `mod` 10]