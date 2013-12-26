dropHelper :: Int->Int->[a]->[a]
dropHelper _ _ [] = []
dropHelper c n (x:xs) = if c == n
	then dropHelper 0 n xs
	else [x] ++ dropHelper (c+1) n xs

myDropEvery :: [a]->Int->[a]
myDropEvery x 0 = x
myDropEvery [] _ = []
myDropEvery x n = dropHelper 0 (n-1) x