module Excersise3 where

one = concat([1,2,3],[4,5,6])
two = (++) [1,2,3] [4,5,6]
three = (++) "hello" "kitty"
four = "hello" !! 4	
five = (!!) "hello" 4	
six = take 4 "lovely"
seven = take 3 "awesone"

position x = (!! x)
myHead = position 0
anIndexOfCurry = ("Curry is awesome" !!)


removeLast x = take (len - 1) x
 				where len = length x



reverseSentence s =  join ' ' (reverse (splitWords s))				

splitWords [] = []
splitWords s = word : splitWords (drop 1 rest) 
			where (word, rest) =  span (/= ' ') s  


join s (x:y:xs) = x ++ [s] ++ join s (y:xs)
join s (x:nil) = x
join s [] = []

main :: IO ()
main = print $ reverseSentence "Curry is Awesome" 