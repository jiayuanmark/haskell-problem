myDuplicate :: [a]->[a]
myDuplicate [] = []
myDuplicate (x:xs) = [x, x] ++ (myDuplicate xs)