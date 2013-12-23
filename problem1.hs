lastElement :: [a]->a 
lastElement [x] = x
lastElement (x:xs) = lastElement xs