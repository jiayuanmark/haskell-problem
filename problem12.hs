data Element a = Multiple Int a | Single a
	deriving (Show)

myDecode :: [Element a]->[a]
myDecode [] = []
myDecode (x:xs) = (decodeHelper x) ++ (myDecode xs)
	where
		decodeHelper (Single y) = [y]
		decodeHelper (Multiple n y) = [y | e<-[1..n]]