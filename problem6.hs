myReverse :: [a]->[a]
myReverse [] = []
myReverse [a] = [a]
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a)=>[a]->Bool
isPalindrome x = (myReverse x) == x