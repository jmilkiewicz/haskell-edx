cmap f xs = [f x | x<-xs]

rmap f [] = []
rmap f (x:xs) = f x : rmap f xs


cfilter f xs = [x | x<- xs, f x]

rfilter f [] = []
rfilter f (x:xs) 
              | f x == True = x : rfilter f xs 
              | otherwise = rfilter f xs 



drawLine s h = [' '| _<- [1..s]] ++ ['#'|_<-[1..h]] 
pyramid n = map (\l -> drawLine  (n-l) l) [1..n]


kDiff xs d = [(x,y)| x<-xs, y<-xs, x-y==d]

fMap f xs = foldr (\x y -> f x : y) [] xs

ffilter f xs = foldr (\x y -> if f x then x : y else y ) [] xs


fsum xs = foldr (+) 0 xs	

f _ v [] = v
f op v (x:xs) = op x (f op v xs)

flength xs = foldr (\_ n -> n+1) 0 xs 

freverse xs = foldr (\x xs -> xs ++ [x]) [] xs
	
allMeets p xs = and (map p xs)
--allMeets p xs = and [p x| x<-xs]

allMeetsC p = and . map p

-- anyMeets p = or . map p
anyMeets p = not . null . filter p

takeWhi _ [] = []
takeWhi p (x:xs)
            | p x = x : takeWhi p xs
            | otherwise = []


dropWhi _ [] = []
dropWhi p (x:xs)
            | p x = dropWhi p xs
            | otherwise = x:xs


filterAndMap p f xs = [f x | x <- xs , p x]
filterAndMapC p f = map f . filter p


compose xs = foldr (.) id xs
--  compose [map (+1), map (*1)]  [1..4] 


unfold p h t x 
	| p x = []
	| otherwise = h x : unfold p h t (t x)

chop4 = unfold null (take 4) (drop 4) 

mapu f xs = unfold null (f . head) tail xs

evens xs = [x|x<-xs, even x]

squares n = [x*x| x<-[1..n]]

sumSquares = sum . squares

squaresP m n = [x*x| x<-[1..n+m], x> n]

sumSquaresP x = sum . uncurry squaresP $ (x, x) 

cords m n = [(x,y) | x<- [0..n], y<- [0..m]]

