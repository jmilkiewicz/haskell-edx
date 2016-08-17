module Print3 where

-- type signature
mygreeting :: String
mygreeting = hello ++ " " ++ world

hello :: String
hello = "hello"

world :: String
world = "world"

main :: IO ()
main = do 
	putStrLn mygreeting
	putStrLn grreting2
	where grreting2 = concat ["hello", " ", "world"]
