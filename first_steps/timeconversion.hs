-- https://www.hackerrank.com/challenges/time-conversion
timeAMPM xs = calc (take 2 xs) (take 6 (drop 2 xs)) (drop 8 xs)


calc "12" r "AM" = "00" ++ r
calc "12" r "PM" = "12" ++ r	
calc h r "AM" = h ++ r
calc h r _ =  addH 12 h ++ r 
strToInt = (read::String->Int)
addH h = show . (+h) . strToInt