-- https://www.hackerrank.com/challenges/cut-the-sticks
cut [] = []
cut xs = length xs : cut (doCut xs) 

doCut = filter (>0) . cutting
cutting xs = map (flip (-) (shortest xs)) xs
shortest = foldr min 10000000 

