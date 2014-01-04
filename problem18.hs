mySlice :: [a]->Int->Int->[a]
mySlice [] _ _ = []
mySlice (x:xs) i j
	| i > 1 = mySlice xs (i-1) (j-1)
	| j < 1 = []
	| otherwise = x : mySlice xs (i-1) (j-1)