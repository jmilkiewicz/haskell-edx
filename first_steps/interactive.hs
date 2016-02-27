import System.IO


read2c = do 
			x<- getChar 
			getChar
			y<- getChar 
			return (x,y)


getWholeLine = do 
				x <- getChar
				if x == '\n' then 
					return []
				else
					do 
						xs <- getWholeLine
						return (x:xs)




putWholeStr [] = return ()
putWholeStr (x:xs) = do 
						putChar x 
						putWholeStr xs	

putWholeStr2 [] = return ()
putWholeStr2 (x:xs) = putChar x  >>  putWholeStr2 xs



putWholeLine l = do
					putWholeStr l
					putChar '\n'

	
coss = putChar 'x'	

newline =  putChar '\\' >> putChar 'n'	


printOutWithSuffix s [] = return s
printOutWithSuffix s (x:xs) = do 
								putChar x
								printOutWithSuffix s xs



myApp = do
					putWholeLine "Enter line"
					x <- getWholeLine
					putWholeStr ("Length of '" ++ x ++ "' is: ")
					putWholeStr (show (length x))						
					putWholeLine " characters"


getWholeLineSecretely = do 
							x <- getChS
							if x == '\n' then
								do 
									putChar x	
									return []
							else
								do 
									putChar '_'
									xs <- getWholeLineSecretely
									return (x:xs)		

getChS = do 
			hSetEcho stdin False
			c <- getChar
			hSetEcho stdin True
			return c


myApp2 = do
			putWholeLine "Enter Word"
			x <- getWholeLineSecretely			
			putWholeLine "ok lets have some fun"
			guess x 1


guess word t = do
				putWholeLine ("Your attempt nr " ++ (show t))
				g <- getWholeLine 
				if g == word then
					putWholeLine ("Success, it is " ++ word)
				else 
					do 
						putWholeLine (diff word g)
						guess word (t+1)	



diff [] _ = []
diff (x:xs) ys 
	| elem x ys = x : diff xs ys 
	| otherwise = '_' : diff xs ys
