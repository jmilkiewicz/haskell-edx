-- https://www.hackerrank.com/challenges/sherlock-and-squares
import Data.Char


--sher a b = length [ x| x <- [1..(b `div` 2)],  x *x <= b && x *x >=a]
--issq n =  (floor (sqrt (fromIntegral n)))  *  (floor (sqrt (fromIntegral n))) == n


--sher :: Int -> Int -> Int

sher a b = 1+ (floor (sqrt (fromIntegral b)))  -  (ceiling (sqrt (fromIntegral a))) 



unfold p h t x 
	| p x = []
	| otherwise = h x : unfold p h t (t x)

parseL  = unfold null takeChunk dropChunk

takeChunk [] = [] 
takeChunk (x:xs) 
	| isSpace x = []
	| otherwise = x : takeChunk xs

dropChunk [] = [] 
dropChunk (x:xs) 
	| isSpace x = xs
	| otherwise = dropChunk xs	



parseAB l = map (\x -> read  x :: Int) (parseL l)





--firstSpaceIndex xs = [i| (x,i)<- zip xs [0..], isSpace  x || i == length xs]
