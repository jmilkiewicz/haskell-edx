-- https://www.hackerrank.com/challenges/diagonal-difference/submissions/code/17609546

diagonals xss = abs (sum (diagonalP xss) - sum (diagonalS xss))

diagonalP xss = diagonal 0 (+1) xss
diagonalS xss = diagonal (length xss -1) (\x->x-1) xss

diagonal _ _ [] = []  
diagonal i t (x:xs) = head (drop i x) : diagonal (t i) t xs  

-- option2 
-- brakuje posumowac w tuplach i odjac z abs
-- diagonals2 xss = unzip [ ( head (drop i x'), head (drop (length xss -1 - i) x') ) | (x',i) <- zip xss [0..(length xss)]]