myReverse :: [a]->[a]
myReverse [] = []
myReverse [a] = [a]
myReverse (x:xs) = (myReverse xs) ++ [x]