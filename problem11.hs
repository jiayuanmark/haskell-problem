data Element a = Multiple Int a | Single a
	deriving (Show)

myLength :: [a]->Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myPack :: (Eq a)=>[a]->[[a]]
myPack [] = []
myPack [x] = [[x]]
myPack (x:xs) = if x `elem` (head (myPack xs))
	then (x:(head (myPack xs))):(tail (myPack xs))
	else [x]:(myPack xs)

myModifiedEncode :: (Eq a)=>[a]->[Element a]
myModifiedEncode [] = []
myModifiedEncode xst = [ y | x<-lst, let y = if (myLength x) == 1 then Single (head x) else Multiple (myLength x) (head x) ]
	where lst = myPack xst