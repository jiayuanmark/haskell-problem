myReplicate :: [a]->Int->[a]
myReplicate [] _ = []
myReplicate (x:xs) n = [x | e<-[1..n]] ++ (myReplicate xs n)