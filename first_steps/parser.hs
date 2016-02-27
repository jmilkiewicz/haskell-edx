item = \inp -> case inp of
				[] -> []
				(x:xs) -> [(x,xs)]	

failure = \_-> []

ret v = \inp -> [(v,inp)]				

p +++ q = \inp -> case p inp of
			[] -> q inp
			[(v,r)] -> [(v,r)]


--parse :: Parser a -> String -> [(a,String)]
parse p inp =  p inp


-- p :: Parser (Char,Char)
p = do {
		x <- item; 
		item; 
		y <- item; 
		ret (x,y)
	}    


isEqualTo x = (x ==)