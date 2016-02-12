factorial 0 = 1
factorial n = n * factorial (n-1)


prod [] = 1
prod (x:xs) = x * prod xs

len [] = 0
len (_:xs) = 1 + len xs

rev [] = []
rev (x:xs) = rev xs ++ [x]



mzip [] _ = []
mzip _ [] = []
mzip (x:xs) (y:ys) = [(x,y)] ++ mzip xs ys -- (x,y) : mzip xs ys

mhalve xs = ([x|x<- take x xs ], [y|y<-drop  x xs ]) where x = length xs `div` 2

dropnth 0 (x:xs) = xs
dropnth i [] = []
dropnth i (x:xs) = x : dropnth (i-1) xs 

dropFn 0 xs = xs
dropFn _ [] = []
dropFn n (_:xs) = dropFn (n-1) xs 

app xs [] = xs
app xs (y:ys) = app (xs ++ [y]) ys

q1 [] = []
q1 (h:xs) =  q1 [x| x<-xs,x<=h ] ++ [h] ++ q1 [x|x<-xs,x>h]


rep n e = [e | _<-[1..n]]

repr 0 _ = []
repr n x = x: repr (n-1) x


nth 0 (x:_) = x 
--nth _ [] =  
nth n (_:xs) = nth (n-1) xs

contains _ [] = False
contains x (y:ys) = x==y || contains x ys

myconcat xss = [x | xs<- xss, x<- xs]

myconcatr [] = []; 
myconcatr (xs:xst) = xs ++ myconcatr xst

d2l 0 = []
d2l n = d2l (n `div` 10) ++ [n `mod` 10]

l2d xs = foldl (\x y -> y + (x*10) ) 0 xs


domagic _ [] = []
domagic False (x:xs) = x : domagic True xs  
domagic True (x:xs) = x*2 : domagic False xs 

magic n = domagic False (reverse (d2l n))	

splitMagic xs = concat [ d2l x| x<-xs]
isValid n = (sum (splitMagic (magic n))) `mod` 10 == 0 


