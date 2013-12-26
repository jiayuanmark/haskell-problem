splitHelper :: ([a], [a])->Int->([a], [a])
splitHelper x n = let (x1, x2) = x 
	in if (length x1) == n
		then (x1, x2) 
		else if (length x2) == 0
			then (x1, x2)
			else splitHelper (x1++[head x2], tail x2) n

mySplit :: [a]->Int->([a], [a])
mySplit x n = splitHelper ([], x) n