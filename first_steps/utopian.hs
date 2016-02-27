-- https://www.hackerrank.com/challenges/utopian-tree

-- foldl (\x y-> y x) 1 (utopian 5 'w' )
utopian 0 _ = []
utopian x 'w' = (*2) : utopian (x-1) 's'
utopian x 's' = (+1) : utopian (x-1) 'w'



-- utopian2 4 'w' 1
utopian2 0 _ = id
utopian2 x 'w' = (utopian2 (x-1) 's') . (*2) 
utopian2 x 's' = (utopian2 (x-1) 'w') . (+1)