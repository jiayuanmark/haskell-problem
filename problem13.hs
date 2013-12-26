data Element a = Multiple Int a | Single a
	deriving (Show)

encodeHelper :: (Eq a)=>Int->a->[a]->[Element a]
encodeHelper 0 _ [] = []
encodeHelper n x [] = if n == 1
	then [Single x]
	else [Multiple n x] 
encodeHelper n x (y:ys) = if x == y
	then encodeHelper (n+1) x ys
	else (encodeHelper n x []) ++ (encodeHelper 1 y ys)

myEncodeDirect :: (Eq a)=>[a]->[Element a]
myEncodeDirect x = encodeHelper 0 (head x) x