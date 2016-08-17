printInc x = print plusOne
 where plusOne = x + 1

printInc2 x = let plusOne = x + 1 
               in print plusOne



-- indentation !!!
multi = x * y
 where x = 5 
       y = 6	               

area r = pi  * r * r
 where pi = 3.14
       
