double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns  
twice f x = f (f x)
twice2 x f = f (f x)	

myAnd True b = b
myAnd False _ = False
 
mySec (x:[]) = x
mySec (_:xs) = head xs

myinc x = x+1

myconst x = \_ -> x

odds n = map (\x -> x*2 +1) [0..n-1]

-- halve xs = (take n xs , drop n xs)
--	where n = length xs `div` 2


-- safetail [] = []
-- safetail (_:xs) = xs 


-- safetail xs | length xs == 0 = xs
--            | otherwise = tail xs

-- safetail xs  = if length xs == 0 then [] else tail xs            
safetail xs  = if null xs then [] else tail xs            

remove x xs = take x xs ++ drop (x+1) xs

--[x| x<- [1..10], (\x -> mod x 2 == 0 ) x]
-- lub 
--[x| x<- [1..10], mod x 2 == 0 ]

factors n = [x | x<- [1..n], n `mod` x == 0]
isPrime x = factors x == [1,x]

primes f = [x| x<- [1..f], isPrime x]

positions n xs = [ i |  (x',i)<- zip xs [0..(length xs)],  x' == n  ] 

pairs xs = zip xs (tail xs)

secretSanta xs = zip xs (tail xs ++ take 1 xs)

isSorted xs = null [(x,y)| (x,y)<- pairs xs, x > y]

-- better
--isSorted xs =  and [x<=y| (x,y)<- pairs xs]

perfects n = [x| x<- [1..n] , sum (factors x) == x*2 ]

-- perfects n = [x| x<- [1..n] , sum (init (factors x)) == x ]

 riffle xs ys =  concat [ x:[y] | (x,y)<- zip xs ys ]
 -- lub
 riffle xs ys = concat [ [x,y] | (x,y)<- zip xs ys ]
