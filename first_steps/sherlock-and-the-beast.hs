-- 	https://www.hackerrank.com/challenges/sherlock-and-the-beast

sherlock 0 = []
sherlock 1 = [-1]
sherlock 2 = [-1]
sherlock 4 = [-1]
sherlock 7 = [-1]
sherlock 5 = times 5 3
sherlock n 
	| n `mod` 3 == 0 = times n 5 
	| n `mod` 3 == 2 = sherlock (3 * ((n `div` 3) -1) )  ++ sherlock 5 
	| otherwise =      sherlock (3 * ((n `div` 3) -3) )  ++ sherlock 5 ++ sherlock 5 


beast x = l2d (sherlock x)

l2d xs = foldl (\x y -> y + (x*10) ) 0 xs