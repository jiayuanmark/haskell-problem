removeDup :: Eq a=>[a]->a->[a]
removeDup [] _ = []
removeDup (x:xs) y = if x == y
	then (removeDup xs x)
	else [x] ++ (removeDup xs x)

myCompress :: Eq a=>[a]->[a]
myCompress [] = []
myCompress (x:xs) = [x] ++ (removeDup xs x)