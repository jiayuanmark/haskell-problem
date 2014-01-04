splitHelper :: ([a], [a])->Int->([a], [a])
splitHelper x n = let (x1, x2) = x 
	in if (length x1) == n
		then (x1, x2) 
		else if (length x2) == 0
			then (x1, x2)
			else splitHelper (x1++[head x2], tail x2) n

mySplit :: [a]->Int->([a], [a])
mySplit x n = splitHelper ([], x) n

myReverse :: [a]->[a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

myRotate :: [a]->Int->[a]
myRotate x n = myReverse ((myReverse a) ++ (myReverse b))
	where (a, b) = mySplit x (if n > 0 then n else (length x)+n) 
