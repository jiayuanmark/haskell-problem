myLength :: [a]->Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myPack :: (Eq a)=>[a]->[[a]]
myPack [] = []
myPack [x] = [[x]]
myPack (x:xs) = if x `elem` (head (myPack xs))
	then (x:(head (myPack xs))):(tail (myPack xs))
	else [x]:(myPack xs)

myEncode :: (Eq a)=>[a]->[(Int, a)]
myEncode [] = []
myEncode x = [ (myLength ele, head ele) | ele <- l ]
	where l = myPack x