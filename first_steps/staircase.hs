-- https://www.hackerrank.com/challenges/staircase
staircase x =[ buildLine (x - l) l| l <-[1..x] ]
buildLine s l =  [ ' ' | _ <-[1..s] ] ++ [ '#' |  _ <-[1..l] ] 


-- to call it 
  {-- 
  let result = staircase n
  putStr ( unlines result )
   --}